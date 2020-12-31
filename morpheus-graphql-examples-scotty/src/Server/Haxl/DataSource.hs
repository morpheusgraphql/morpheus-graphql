{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Haxl.DataSource
  ( Deity (..),
    State (DeityState),
    Haxl,
    getDeityIds,
    getNameById,
    getPowerById,
  )
where

import Control.Monad
import Data.Hashable
import Data.Morpheus.Types
  ( ID (..),
    ResolverQ,
    lift,
  )
import Data.Text (Text)
import Data.Typeable
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
    putSuccess,
  )
import Server.Haxl.Schema
  ( Deity (..),
  )

type Haxl = GenHaxl () ()

data DeityReq a where
  GetAllIds :: DeityReq [ID]
  GetNameById :: ID -> DeityReq Text
  GetPowerById :: ID -> DeityReq (Maybe Text)
  deriving (Typeable)

deriving instance Eq (DeityReq a)

instance Hashable (DeityReq a) where
  hashWithSalt s GetAllIds = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetNameById a) = hashWithSalt s (1 :: Int, a)
  hashWithSalt s (GetPowerById a) = hashWithSalt s (2 :: Int, a)

deriving instance Show (DeityReq a)

instance ShowP DeityReq where showp = show

instance StateKey DeityReq where
  data State DeityReq = DeityState

instance DataSourceName DeityReq where
  dataSourceName _ = "DeityDataSource"

instance DataSource u DeityReq where
  fetch _ _ _ = BackgroundFetch myfetch

fetchDeityIds :: IO [ID]
fetchDeityIds = pure $ map ID ["Morpheus", "Zeus", "Ares"]

fetchDeityNames :: [ID] -> IO [Text]
fetchDeityNames ids = do
  print ("Fetch Name for: " <> show ids)
  pure (map unpackID ids)

fetchDeityPower :: [ID] -> IO [Maybe Text]
fetchDeityPower ids = do
  print ("Fetch Power for: " <> show ids)
  pure $ map (const $ Just "Shapeshifting") ids

fetchAll :: Foldable t => t (ResultVar [ID]) -> IO ()
fetchAll allIdVars = do
  allIds <- fetchDeityIds
  mapM_ (`putSuccess` allIds) allIdVars

handleBatched :: ([i] -> IO [a]) -> [(i, ResultVar a)] -> IO ()
handleBatched f ls = unless (null ids) $ do
  names <- f ids
  mapM_ (uncurry putSuccess) (zip vars names)
  where
    (ids, vars) = unzip ls

myfetch ::
  [BlockedFetch DeityReq] ->
  IO ()
myfetch blockedFetches = do
  unless (null allIdVars) (fetchAll allIdVars)
  handleBatched fetchDeityNames [(uid, r) | BlockedFetch (GetNameById uid) r <- blockedFetches]
  handleBatched fetchDeityPower [(uid, r) | BlockedFetch (GetPowerById uid) r <- blockedFetches]
  where
    allIdVars :: [ResultVar [ID]]
    allIdVars = [r | BlockedFetch GetAllIds r <- blockedFetches]

getDeityIds :: ResolverQ e Haxl [ID]
getDeityIds = lift $ dataFetch GetAllIds

getNameById :: ID -> ResolverQ e Haxl Text
getNameById userId = lift $ dataFetch (GetNameById userId)

getPowerById :: ID -> ResolverQ e Haxl (Maybe Text)
getPowerById userId = lift $ dataFetch (GetPowerById userId)
