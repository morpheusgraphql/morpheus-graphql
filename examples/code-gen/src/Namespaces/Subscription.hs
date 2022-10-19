{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Namespaces.Subscription where

import Data.Data (Typeable)
import Data.Morpheus ()
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Globals.GQLScalars

data Query m = Query
  { queryDeity :: QueryDeityArgs -> m (Deity m),
    queryCharacter :: QueryCharacterArgs -> m (Character m),
    queryHero :: m (Human m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Query"}

data QueryDeityArgs = QueryDeityArgs
  { queryDeityArgsName :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    queryDeityArgsMythology :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType QueryDeityArgs where
  type KIND QueryDeityArgs = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "QueryDeityArgs"}

data QueryCharacterArgs = QueryCharacterArgs
  { queryCharacterArgsCharacterID :: Text,
    queryCharacterArgsAge :: Maybe Int
  }
  deriving (Generic, Show)

instance GQLType QueryCharacterArgs where
  type KIND QueryCharacterArgs = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "QueryCharacterArgs"}

data Mutation m = Mutation
  { mutationCreateDeity :: MutationCreateDeityArgs -> m (Deity m),
    mutationCreateCharacter :: MutationCreateCharacterArgs -> m (Character m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Mutation m) where
  type KIND (Mutation m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Mutation"}

data MutationCreateDeityArgs = MutationCreateDeityArgs
  { mutationCreateDeityArgsDeityName :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    mutationCreateDeityArgsDeityMythology :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType MutationCreateDeityArgs where
  type KIND MutationCreateDeityArgs = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "MutationCreateDeityArgs"}

data MutationCreateCharacterArgs = MutationCreateCharacterArgs
  { mutationCreateCharacterArgsCharRealm :: Realm,
    mutationCreateCharacterArgsCharMutID :: Text
  }
  deriving (Generic, Show)

instance GQLType MutationCreateCharacterArgs where
  type KIND MutationCreateCharacterArgs = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "MutationCreateCharacterArgs"}

data Subscription m = Subscription
  { subscriptionNewDeity :: m (Deity m),
    subscriptionNewCharacter :: m (Character m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Subscription m) where
  type KIND (Subscription m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Subscription"}

data Character m
  = CharacterCreature (Creature m)
  | CharacterDeity (Deity m)
  | CharacterHuman (Human m)
  deriving (Generic)

instance (Typeable m) => GQLType (Character m) where
  type KIND (Character m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Character"}

data Deity m = Deity
  { deityFullName :: m Text,
    deityPower :: m Power
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Deity"}

data Creature m = Creature
  { creatureCreatureName :: m Text,
    creatureRealm :: m City
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Creature m) where
  type KIND (Creature m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Creature"}
      <> fieldDirective "creatureCreatureName" Describe {text = "\n  simple field description\n  "}

data Human m = Human
  { humanHumanName :: m Text,
    humanProfession :: m (Maybe Text)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Human m) where
  type KIND (Human m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Human"}

data Realm = Realm
  { realmOwner :: Text,
    realmPlace :: Maybe Int
  }
  deriving (Generic, Show)

instance GQLType Realm where
  type KIND Realm = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Realm"}

data City
  = CityAthens
  | CityIthaca
  | CitySparta
  | CityTroy
  deriving (Generic, Show)

instance GQLType City where
  type KIND City = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "City"}

type Power = Int
