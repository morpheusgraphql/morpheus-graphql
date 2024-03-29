{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Operation.Query where

import Data.Morpheus.Server.CodeGen.Internal
import Data.Morpheus.Server.Types
import Globals.GQLScalars (ScalarPower)

data Query m = Query
  { deity :: DeityArgs -> m (Deity m),
    character :: CharacterArgs -> m (Character m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
  directives _ =
    typeDirective Describe {text = "\nmy interface description\n"}

data DeityArgs = DeityArgs
  { name :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    id :: ID
  }
  deriving (Generic, Show)

instance GQLType DeityArgs where
  type KIND DeityArgs = TYPE

data CharacterArgs = CharacterArgs
  { characterID :: Text,
    age :: Maybe Int
  }
  deriving (Generic, Show)

instance GQLType CharacterArgs where
  type KIND CharacterArgs = TYPE

data Character m
  = CharacterCreature (m (Creature m))
  | CharacterDeity (m (Deity m))
  deriving (Generic)

instance (Typeable m) => GQLType (Character m) where
  type KIND (Character m) = TYPE

data Deity m = Deity
  { fullName :: m Text,
    power :: m (Maybe ScalarPower)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE
  directives _ =
    fieldDirective "fullName" Describe {text = "\n  simple field description\n  "}

data Creature m = Creature
  { creatureName :: m Text,
    realm :: m City,
    immortality :: m Bool
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Creature m) where
  type KIND (Creature m) = TYPE

data City
  = Athens
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic, Show)

instance GQLType City where
  type KIND City = TYPE
