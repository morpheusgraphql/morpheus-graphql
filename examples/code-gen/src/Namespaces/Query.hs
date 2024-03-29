{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Namespaces.Query where

import Data.Morpheus.Server.CodeGen.Internal
import Data.Morpheus.Server.Types
import Globals.GQLScalars (ScalarPower)

data Query m = Query
  { queryDeity :: QueryDeityArgs -> m (Deity m),
    queryCharacter :: QueryCharacterArgs -> m (Character m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Query"}
      <> typeDirective Describe {text = "\nmy interface description\n"}

data QueryDeityArgs = QueryDeityArgs
  { queryDeityArgsName :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    queryDeityArgsId :: ID
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

data Character m
  = CharacterCreature (m (Creature m))
  | CharacterDeity (m (Deity m))
  deriving (Generic)

instance (Typeable m) => GQLType (Character m) where
  type KIND (Character m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Character"}

data Deity m = Deity
  { deityFullName :: m Text,
    deityPower :: m (Maybe ScalarPower)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Deity"}
      <> fieldDirective "deityFullName" Describe {text = "\n  simple field description\n  "}

data Creature m = Creature
  { creatureCreatureName :: m Text,
    creatureRealm :: m City,
    creatureImmortality :: m Bool
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Creature m) where
  type KIND (Creature m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Creature"}

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
