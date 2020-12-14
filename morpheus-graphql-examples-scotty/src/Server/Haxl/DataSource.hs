{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Haxl.DataSource
  ( Deity (..),
    Realm (..),
    State (DeityState),
    Haxl,
    getAllDeityIds,
    getDeityById,
    Id,
  )
where

import Control.Monad
import Data.Hashable
import Data.Morpheus.Types
  ( ResolverQ,
    lift,
  )
import Data.Text (Text)
import Data.Typeable
import Debug.Trace
import Haxl.Core
import Server.Haxl.Schema
  ( Deity (..),
    Realm (..),
  )

type Haxl = GenHaxl () ()

type Id = Text

data DeityReq a where
  GetAllIds :: DeityReq [Id]
  GetDeityById :: Id -> DeityReq Deity
  deriving (Typeable)

deriving instance Eq (DeityReq a)

instance Hashable (DeityReq a) where
  hashWithSalt s GetAllIds = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetDeityById a) = hashWithSalt s (1 :: Int, a)

deriving instance Show (DeityReq a)

instance ShowP DeityReq where showp = show

instance StateKey DeityReq where
  data State DeityReq = DeityState

instance DataSourceName DeityReq where
  dataSourceName _ = "DeityDataSource"

instance DataSource u DeityReq where
  fetch _ _ _ = BackgroundFetch myfetch

sqlSingle :: (Applicative m) => m [Id]
sqlSingle =
  pure
    [ "Morpheus",
      "Zeus",
      "Ares"
    ]

fetchDeities :: Applicative m => [Id] -> m [Deity]
fetchDeities = traverse single . traceShowId
  where
    single name =
      pure
        Deity
          { name,
            power = Just "Shapeshifting",
            realm = Dream,
            bornAt = Just Olympus
          }

fetchAll :: Foldable t => t (ResultVar [Id]) -> IO ()
fetchAll allIdVars = do
  allIds <- sqlSingle
  mapM_ (`putSuccess` allIds) allIdVars

handleList :: (t -> IO [a]) -> t -> [ResultVar a] -> IO ()
handleList f ids vars = do
  names <- f ids
  mapM_ (uncurry putSuccess) (zip vars names)

myfetch ::
  [BlockedFetch DeityReq] ->
  IO ()
myfetch blockedFetches = do
  unless (null allIdVars) (fetchAll allIdVars)
  unless (null ids) $ handleList fetchDeities ids vars
  where
    allIdVars :: [ResultVar [Id]]
    allIdVars = [r | BlockedFetch GetAllIds r <- blockedFetches]
    ids :: [Id]
    vars :: [ResultVar Deity]
    (ids, vars) = unzip [(userId, r) | BlockedFetch (GetDeityById userId) r <- blockedFetches]

getAllDeityIds :: ResolverQ e Haxl [Id]
getAllDeityIds = lift $ dataFetch GetAllIds

getDeityById :: Id -> ResolverQ e Haxl Deity
getDeityById userId = lift $ dataFetch (GetDeityById userId)
