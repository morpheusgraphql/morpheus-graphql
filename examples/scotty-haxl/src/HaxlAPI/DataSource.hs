{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module HaxlAPI.DataSource
  ( Deity (..),
    State (DeityState),
    Haxl,
    DeityReq (..),
  )
where

import Control.Monad
import Data.Hashable
import Data.Morpheus.Types
  ( ID (..),
  )
import Data.Semigroup ((<>))
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
    putSuccess,
  )
import HaxlAPI.Schema
  ( Deity (..),
  )
import Prelude
  ( Eq (..),
    Foldable (..),
    IO,
    Int,
    Maybe (..),
    Show (..),
    String,
    const,
    map,
    print,
    pure,
    uncurry,
    unzip,
    zip,
    ($),
  )

type Haxl = GenHaxl () ()

data DeityReq a where
  GetDeityIds :: DeityReq [ID]
  GetDeityNameById :: ID -> DeityReq Text
  GetDeityPowerById :: ID -> DeityReq (Maybe Text)
  deriving (Typeable)

deriving instance Eq (DeityReq a)

instance Hashable (DeityReq a) where
  hashWithSalt s GetDeityIds = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetDeityNameById a) = hashWithSalt s (1 :: Int, a)
  hashWithSalt s (GetDeityPowerById a) = hashWithSalt s (2 :: Int, a)

deriving instance Show (DeityReq a)

instance ShowP DeityReq where showp = show

instance StateKey DeityReq where
  data State DeityReq = DeityState

instance DataSourceName DeityReq where
  dataSourceName _ = "DeityDataSource"

instance DataSource u DeityReq where
  fetch _ _ _ = BackgroundFetch myfetch

fetchAll :: (Foldable t) => t (ResultVar [ID]) -> IO ()
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
  handleBatched fetchDeityNames [(uid, r) | BlockedFetch (GetDeityNameById uid) r <- blockedFetches]
  handleBatched fetchDeityPowers [(uid, r) | BlockedFetch (GetDeityPowerById uid) r <- blockedFetches]
  where
    allIdVars :: [ResultVar [ID]]
    allIdVars = [r | BlockedFetch GetDeityIds r <- blockedFetches]

-- Fetch

fetchDeityIds :: IO [ID]
fetchDeityIds = do
  print ("Fetch Ids" :: String)
  pure ["Morpheus", "Zeus", "Ares"]

fetchDeityNames :: [ID] -> IO [Text]
fetchDeityNames ids = do
  print ("Fetch Name for: " <> show ids)
  pure (map unpackID ids)

fetchDeityPowers :: [ID] -> IO [Maybe Text]
fetchDeityPowers ids = do
  print ("Fetch Power for: " <> show ids)
  pure $ map (const $ Just "Shapeshifting") ids
