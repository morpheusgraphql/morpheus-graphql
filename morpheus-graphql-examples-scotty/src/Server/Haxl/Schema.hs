{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server.Haxl.Schema
  ( Deity (..),
    Realm (..),
    Query (..),
    DeityArgs (..),
  )
where

import Data.Morpheus.Types
  ( GQLType (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Realm
  = Olympus
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

data DeityArgs = DeityArgs
  { name :: Text,
    bornPlace :: Maybe Realm
  }
  deriving (Generic, GQLType)

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    deities :: m [Deity]
  }
  deriving (Generic, GQLType)
