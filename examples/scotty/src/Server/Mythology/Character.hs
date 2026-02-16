{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Mythology.Character
  ( Deity (..),
    dbDeity,
    Human (..),
    someHuman,
    someDeity,
    Person,
    PersonGuard,
    resolvePersons,
  )
where

import Data.Morpheus.Types (GQLType (..), TypeGuard (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Server.Mythology.Place
  ( City (..),
    Realm (..),
  )

newtype Person = Person {name :: Text}
  deriving (Generic, GQLType)

data Deity = Deity
  { name :: Text, -- Non-Nullable Field
    power :: Maybe Text, -- Nullable Field
    realm :: Realm,
    bornAt :: Maybe City
  }
  deriving (Generic, GQLType)

data Human m = Human
  { name :: m Text,
    bornAt :: m City
  }
  deriving (Generic, GQLType)

type PersonGuard m = TypeGuard Person (UnionPerson m)

resolvePersons :: (Applicative m) => [PersonGuard m]
resolvePersons = ResolveType <$> [UnionPersonDeity someDeity, UnionPersonHuman someHuman]

data UnionPerson m
  = UnionPersonDeity Deity
  | UnionPersonHuman (Human m)
  deriving (Generic, GQLType)

someHuman :: (Applicative m) => Human m
someHuman = Human {name = pure "Odysseus", bornAt = pure Ithaca}

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
  return $
    Right $
      Deity
        { name = "Morpheus",
          power = Just "Shapeshifting",
          realm = Dream,
          bornAt
        }
