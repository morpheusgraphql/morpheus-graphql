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
    City (..),
    dbDeity,
    State (EmptyState),
  )
where

import Control.Monad
import Data.Hashable
import Data.Morpheus.Types (GQLType (..))
import Data.Text (Text)
import Data.Typeable
import GHC.Generics (Generic)
import Haxl.Core

type Id = Int

data UserReq a where
  GetAllIds :: UserReq [Id]
  GetNameById :: Id -> UserReq Name
  deriving (Typeable)

type Name = String

deriving instance Eq (UserReq a)

instance Hashable (UserReq a) where
  hashWithSalt s GetAllIds = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetNameById a) = hashWithSalt s (1 :: Int, a)

deriving instance Show (UserReq a)

instance ShowP UserReq where showp = show

instance StateKey UserReq where
  data State UserReq = EmptyState

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

instance DataSource u UserReq where
  fetch _ _ _ = SyncFetch myfetch

sqlSingle :: a
sqlSingle = undefined

sqlMulty :: Applicative m => [Id] -> m [a]
sqlMulty _ = pure []

fetchAll :: Foldable t => t (ResultVar a) -> IO ()
fetchAll allIdVars = do
  allIds <- sqlSingle
  mapM_ (`putSuccess` allIds) allIdVars

fetchList :: [Id] -> [ResultVar a] -> IO ()
fetchList ids vars = do
  names <- sqlMulty ids
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
    vars :: [ResultVar Name]
    (ids, vars) = unzip [(userId, r) | BlockedFetch (GetNameById userId) r <- blockedFetches]

data Deity = Deity
  { name :: Text,
    power :: Maybe Text,
    realm :: Realm,
    bornAt :: Maybe City
  }
  deriving (Generic, GQLType)

dbDeity :: Text -> Maybe City -> IO (Either String Deity)
dbDeity _ bornAt =
  return $ Right $
    Deity
      { name = "Morpheus",
        power = Just "Shapeshifting",
        realm = Dream,
        bornAt
      }

data Realm
  = MountOlympus
  | Sky
  | Sea
  | Underworld
  | Dream
  deriving (Generic, GQLType)

data City
  = Athens
  | Colchis
  | Delphi
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic, GQLType)
