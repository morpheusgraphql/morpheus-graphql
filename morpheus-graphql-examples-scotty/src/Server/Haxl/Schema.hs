{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Haxl.Schema
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
  ( GQLType (..),
    ResolverQ,
    lift,
  )
import Data.Text (Text)
import Data.Typeable
import GHC.Generics (Generic)
import Haxl.Core

type Haxl = GenHaxl () ()

type Id = Text

data UserReq a where
  GetAllIds :: UserReq [Id]
  GetNameById :: Id -> UserReq Deity
  deriving (Typeable)

deriving instance Eq (UserReq a)

instance Hashable (UserReq a) where
  hashWithSalt s GetAllIds = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetNameById a) = hashWithSalt s (1 :: Int, a)

deriving instance Show (UserReq a)

instance ShowP UserReq where showp = show

instance StateKey UserReq where
  data State UserReq = DeityState

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

instance DataSource u UserReq where
  fetch _ _ _ = SyncFetch myfetch

sqlSingle :: (Applicative m) => m [Id]
sqlSingle = pure ["Morpheus"]

fetchDeities :: Applicative m => [Id] -> m [Deity]
fetchDeities = traverse single
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

fetchList :: [Id] -> [ResultVar Deity] -> IO ()
fetchList = handleList fetchDeities

handleList :: (t -> IO [a]) -> t -> [ResultVar a] -> IO ()
handleList f ids vars = do
  names <- f ids
  mapM_ (uncurry putSuccess) (zip vars names)

myfetch ::
  [BlockedFetch UserReq] ->
  IO ()
myfetch blockedFetches = do
  unless (null allIdVars) (fetchAll allIdVars)
  unless (null ids) $ fetchList ids vars
  where
    allIdVars :: [ResultVar [Id]]
    allIdVars = [r | BlockedFetch GetAllIds r <- blockedFetches]
    ids :: [Id]
    vars :: [ResultVar Deity]
    (ids, vars) = unzip [(userId, r) | BlockedFetch (GetNameById userId) r <- blockedFetches]

data Realm
  = Olympus
  | Sky
  | Sea
  | Underworld
  | Dream
  deriving (Generic, Show, GQLType)

data Deity = Deity
  { name :: Text,
    power :: Maybe Text,
    realm :: Realm,
    bornAt :: Maybe Realm
  }
  deriving (Generic, Show, GQLType)

getAllDeityIds :: ResolverQ e Haxl [Id]
getAllDeityIds = lift $ dataFetch GetAllIds

getDeityById :: Id -> ResolverQ e Haxl Deity
getDeityById userId = lift $ dataFetch (GetNameById userId)
