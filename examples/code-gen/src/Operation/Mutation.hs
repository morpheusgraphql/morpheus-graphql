{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Operation.Mutation where

import Data.Data (Typeable)
import Data.Morpheus ()
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)

---- GQL Query -------------------------------
data Query m = Query
  { deity :: DeityArgs -> m (Deity m),
    character :: CharacterArgs -> m (Character m),
    hero :: m (Human m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE

---- GQL DeityArgs -------------------------------
data DeityArgs = DeityArgs
  { name :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    mythology :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType DeityArgs where
  type KIND DeityArgs = TYPE

---- GQL CharacterArgs -------------------------------
data CharacterArgs = CharacterArgs
  { characterID :: Text,
    age :: Maybe Int
  }
  deriving (Generic, Show)

instance GQLType CharacterArgs where
  type KIND CharacterArgs = TYPE

---- GQL Mutation -------------------------------
data Mutation m = Mutation
  { createDeity :: CreateDeityArgs -> m (Deity m),
    createCharacter :: CreateCharacterArgs -> m (Character m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Mutation m) where
  type KIND (Mutation m) = TYPE

---- GQL CreateDeityArgs -------------------------------
data CreateDeityArgs = CreateDeityArgs
  { deityName :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    deityMythology :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType CreateDeityArgs where
  type KIND CreateDeityArgs = TYPE

---- GQL CreateCharacterArgs -------------------------------
data CreateCharacterArgs = CreateCharacterArgs
  { charRealm :: Realm,
    charMutID :: Text
  }
  deriving (Generic, Show)

instance GQLType CreateCharacterArgs where
  type KIND CreateCharacterArgs = TYPE

---- GQL Character -------------------------------
data Character m
  = CharacterCreature
      { unCharacterCreature :: Creature m
      }
  | CharacterDeity
      { unCharacterDeity :: Deity m
      }
  | CharacterHuman
      { unCharacterHuman :: Human m
      }
  deriving (Generic)

instance (Typeable m) => GQLType (Character m) where
  type KIND (Character m) = TYPE

---- GQL Deity -------------------------------
data Deity m = Deity
  { fullName :: m Text,
    power :: m Power
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE
  directives _ = fieldDirective "fullName" Describe {text = "\n  simple field description\n  "}

---- GQL Creature -------------------------------
data Creature m = Creature
  { creatureName :: m Text,
    realm :: m City
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Creature m) where
  type KIND (Creature m) = TYPE

---- GQL Human -------------------------------
data Human m = Human
  { humanName :: m Text,
    profession :: m (Maybe Text)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Human m) where
  type KIND (Human m) = TYPE

---- GQL Realm -------------------------------
data Realm = Realm
  { owner :: Text,
    place :: Maybe Int
  }
  deriving (Generic, Show)

instance GQLType Realm where
  type KIND Realm = TYPE

---- GQL City -------------------------------
data City
  = Athens
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic, Show)

instance GQLType City where
  type KIND City = TYPE
  directives _ = enumDirective "Troy" Deprecated {reason = Just "some reason"}

---- GQL Power -------------------------------
type Power = Int
