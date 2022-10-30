{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Data.Morpheus.App.Internal.Resolving.Haxl
  ( State (DeityState),
    Haxl,
    DeityReq (..),
    getNamedIds,
    getNamedResponseById,
    withHaxl,
  )
where

import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable
import Data.Hashable
import qualified Data.Map as M
import Data.Morpheus.Types.Internal.AST (ObjectEntry (..), TypeName, VALID, ValidValue, Value (..))
import qualified Data.Set as S
import Data.Typeable
import GHC.Exts (sortWith)
import Haxl.Core
  ( BlockedFetch (..),
    DataSource (..),
    DataSourceName (..),
    GenHaxl,
    PerformFetch (..),
    ResultVar (..),
    ShowP (..),
    StateKey (..),
    dataFetch,
    initEnv,
    putSuccess,
    runHaxl,
    stateEmpty,
    stateSet,
  )

withHaxl :: GenHaxl () w b -> IO b
withHaxl haxlApp = do
  let stateStore = stateSet DeityState stateEmpty
  environment <- initEnv stateStore ()
  runHaxl environment haxlApp

getNamedIds :: Haxl [NamedArg]
getNamedIds = dataFetch GetDeityIds

getNamedResponseById :: NamedArg -> Haxl NamedResponse
getNamedResponseById = dataFetch . GetDeityNameById

type Haxl = GenHaxl () ()

type NamedArg = (TypeName, ValidValue)

type NamedResponse = ValidValue

data DeityReq a where
  GetDeityIds :: DeityReq [NamedArg]
  GetDeityNameById :: NamedArg -> DeityReq NamedResponse
  deriving (Typeable)

deriving instance Eq (DeityReq a)

instance Hashable ValidValue where
  hashWithSalt s (Object x) = hashWithSalt s (0 :: Int, toList x)
  hashWithSalt s (List x) = hashWithSalt s (1 :: Int, x)
  hashWithSalt s (Enum x) = hashWithSalt s (2 :: Int, x)
  hashWithSalt s (Scalar x) = hashWithSalt s (3 :: Int, show x)
  hashWithSalt s Null = hashWithSalt s (4 :: Int)

instance Hashable (ObjectEntry VALID) where
  hashWithSalt s (ObjectEntry name value) = hashWithSalt s (name, value)

instance Hashable (DeityReq a) where
  hashWithSalt s GetDeityIds = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetDeityNameById a) = hashWithSalt s (1 :: Int, a)

deriving instance Show (DeityReq a)

instance ShowP DeityReq where showp = show

instance StateKey DeityReq where
  data State DeityReq = DeityState

instance DataSourceName DeityReq where
  dataSourceName _ = "DeityDataSource"

instance DataSource u DeityReq where
  fetch _ _ _ = BackgroundFetch myfetch

fetchAll :: Foldable t => t (ResultVar [NamedArg]) -> IO ()
fetchAll allIdVars = do
  allIds <- fetchDeityIds
  mapM_ (`putSuccess` allIds) allIdVars

handleBatched :: ([i] -> IO [a]) -> [(i, ResultVar a)] -> IO ()
handleBatched f ls = unless (null ids) $ do
  names <- f ids
  mapM_ (uncurry putSuccess) (zip vars names)
  where
    (ids, vars) = unzip ls

myfetch :: [BlockedFetch DeityReq] -> IO ()
myfetch blockedFetches = do
  unless (null allIdVars) (fetchAll allIdVars)
  handleBatched fetchValues [(uid, r) | BlockedFetch (GetDeityNameById uid) r <- blockedFetches]
  where
    allIdVars :: [ResultVar [NamedArg]]
    allIdVars = [r | BlockedFetch GetDeityIds r <- blockedFetches]

fetchValues :: [NamedArg] -> IO [ValidValue]
fetchValues ids = do
  let entityTypes = getAllEntityTypes ids
  let indexed = zip [0 .. length ids] ids
  let clusters = map (seletcByEntity indexed) entityTypes
  xs <- concat <$> traverse fethcByTypeName clusters
  pure $ map snd (sortWith fst xs)

getAllEntityTypes :: Ord a => [(a, b)] -> [a]
getAllEntityTypes xs = S.toList (S.fromList (map fst xs))

seletcByEntity :: Eq a => [(Int, (a, b))] -> a -> (a, [(Int, b)])
seletcByEntity xs entityType = (entityType, map (second snd) $ filter (\v -> fst (snd v) == entityType) xs)

fethcByTypeName :: (TypeName, [(Int, ValidValue)]) -> IO [(Int, ValidValue)]
fethcByTypeName (typeName, ids) = do
  let values = map snd ids
  let indexes = map fst ids
  let handler = M.lookup typeName resMap
  xs <- maybe (fail "handler not found") (\f -> f values) handler
  pure (zip indexes xs)

type ResMap = M.Map TypeName ([ValidValue] -> IO [ValidValue])


