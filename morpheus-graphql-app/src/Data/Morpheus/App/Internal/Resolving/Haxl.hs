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
  ( State (..),
    Haxl,
    Req (..),
    getIds,
    getResponseById,
    withHaxl,
    NamedArg,
    NamedResponse,
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

withHaxl :: State Req -> GenHaxl () w b -> IO b
withHaxl state haxlApp = do
  let stateStore = stateSet state stateEmpty
  environment <- initEnv stateStore ()
  runHaxl environment haxlApp

getIds :: Haxl [NamedArg]
getIds = dataFetch GetIds

getResponseById :: NamedArg -> Haxl NamedResponse
getResponseById = dataFetch . GetValueById

type Haxl = GenHaxl () ()

type NamedArg = (TypeName, ValidValue)

type NamedResponse = ValidValue

data Req a where
  GetIds :: Req [NamedArg]
  GetValueById :: NamedArg -> Req NamedResponse
  deriving (Typeable)

deriving instance Eq (Req a)

instance Hashable ValidValue where
  hashWithSalt s (Object x) = hashWithSalt s (0 :: Int, toList x)
  hashWithSalt s (List x) = hashWithSalt s (1 :: Int, x)
  hashWithSalt s (Enum x) = hashWithSalt s (2 :: Int, x)
  hashWithSalt s (Scalar x) = hashWithSalt s (3 :: Int, show x)
  hashWithSalt s Null = hashWithSalt s (4 :: Int)

instance Hashable (ObjectEntry VALID) where
  hashWithSalt s (ObjectEntry name value) = hashWithSalt s (name, value)

instance Hashable (Req a) where
  hashWithSalt s GetIds = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetValueById a) = hashWithSalt s (1 :: Int, a)

deriving instance Show (Req a)

instance ShowP Req where showp = show

instance StateKey Req where
  data State Req = ReqState
    { resMap :: M.Map TypeName ([ValidValue] -> IO [ValidValue]),
      allIds :: IO [NamedArg]
    }

instance DataSourceName Req where
  dataSourceName _ = "DeityDataSource"

instance DataSource u Req where
  fetch state _ _ = BackgroundFetch (appFetch state)

handleBatched :: ([i] -> IO [a]) -> [(i, ResultVar a)] -> IO ()
handleBatched f ls = unless (null ids) $ do
  names <- f ids
  mapM_ (uncurry putSuccess) (zip vars names)
  where
    (ids, vars) = unzip ls

appFetch :: State Req -> [BlockedFetch Req] -> IO ()
appFetch state blockedFetches = do
  ids <- allIds state
  unless (null allIdVars) (mapM_ (`putSuccess` ids) allIdVars)
  handleBatched (fetchValues state) [(uid, r) | BlockedFetch (GetValueById uid) r <- blockedFetches]
  where
    allIdVars :: [ResultVar [NamedArg]]
    allIdVars = [r | BlockedFetch GetIds r <- blockedFetches]

-- Fetch

fetchValues :: State Req -> [NamedArg] -> IO [ValidValue]
fetchValues state ids = do
  let entityTypes = getAllEntityTypes ids
  let indexed = zip [0 .. length ids] ids
  let clusters = map (selectByEntity indexed) entityTypes
  xs <- concat <$> traverse (fetchByTypeName state) clusters
  pure $ map snd (sortWith fst xs)

getAllEntityTypes :: Ord a => [(a, b)] -> [a]
getAllEntityTypes xs = S.toList (S.fromList (map fst xs))

selectByEntity :: Eq a => [(Int, (a, b))] -> a -> (a, [(Int, b)])
selectByEntity xs entityType = (entityType, map (second snd) $ filter (\v -> fst (snd v) == entityType) xs)

fetchByTypeName :: State Req -> (TypeName, [(Int, ValidValue)]) -> IO [(Int, ValidValue)]
fetchByTypeName state (typeName, ids) = do
  let values = map snd ids
  let indexes = map fst ids
  let handler = M.lookup typeName (resMap state)
  xs <- maybe (fail "handler not found") (\f -> f values) handler
  pure (zip indexes xs)
