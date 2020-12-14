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
    Human (..),
    Realm (..),
    City (..),
    dbDeity,
    someHuman,
    someDeity,
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
  data State UserReq = UserState_

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

instance DataSource u UserReq where
  fetch _ _ _ = SyncFetch myfetch

sqlSingle :: Int -> a
sqlSingle _ = undefined

sqlMulty :: [Id] -> m [a]
sqlMulty _ = undefined

myfetch ::
  [BlockedFetch UserReq] ->
  IO ()
myfetch blockedFetches = do
  unless (null allIdVars) $ do
    allIds <- sqlSingle 1
    mapM_ (`putSuccess` allIds) allIdVars
  unless (null ids) $ do
    names <- sqlMulty ids
    mapM_ (uncurry putSuccess) (zip vars names)
  where
    allIdVars :: [ResultVar [Id]]
    allIdVars = [r | BlockedFetch GetAllIds r <- blockedFetches]
    ids :: [Id]
    vars :: [ResultVar Name]
    (ids, vars) =
      unzip
        [(userId, r) | BlockedFetch (GetNameById userId) r <- blockedFetches]

data Deity = Deity
  { name :: Text,
    power :: Maybe Text,
    realm :: Realm,
    bornAt :: Maybe City
  }
  deriving (Generic, GQLType)

data Human m = Human
  { name :: m Text,
    bornAt :: m City
  }
  deriving (Generic, GQLType)

someHuman :: Applicative m => Human m
someHuman =
  Human
    { name = pure "Odysseus",
      bornAt = pure Ithaca
    }

someDeity :: Deity
someDeity =
  Deity
    { name = "Morpheus",
      power = Just "Shapeshifting",
      realm = Dream,
      bornAt = Nothing
    }

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
