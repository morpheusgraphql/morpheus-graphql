{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Simple where

import Data.Data (Typeable)
import Data.Map (empty, fromList)
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)

---- GQL Query -------------------------------
data Query m = Query
  { deity :: DeityArgs -> m (Deity m),
    character :: CharacterArgs -> m (Character m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
  description _ = Just "\nmy interface description\n"

---- GQL DeityArgs -------------------------------
data DeityArgs = DeityArgs
  { name :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    id :: ID
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

---- GQL Character -------------------------------
data Character m
  = CharacterCreature
      { unCharacterCreature :: Creature m
      }
  | CharacterDeity
      { unCharacterDeity :: Deity m
      }
  deriving (Generic)

instance (Typeable m) => GQLType (Character m) where
  type KIND (Character m) = TYPE

---- GQL Deity -------------------------------
data Deity m = Deity
  { fullName :: m Text,
    power :: m (Maybe Power)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE
  getDescriptions _ = fromList [("fullName", "\n  simple field description\n  ")]

---- GQL Creature -------------------------------
data Creature m = Creature
  { creatureName :: m Text,
    realm :: m City,
    immortality :: m Bool
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Creature m) where
  type KIND (Creature m) = TYPE

---- GQL City -------------------------------
data City
  = Athens
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic, Show)

instance GQLType City where
  type KIND City = TYPE

---- GQL Power -------------------------------
type Power = Int
