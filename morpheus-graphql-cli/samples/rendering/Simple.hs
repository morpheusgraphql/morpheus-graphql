{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Simple where

import Data.Morpheus.Kind (ENUM, INPUT, OBJECT, SCALAR, UNION)
import Data.Morpheus.Types (GQLScalar (..), GQLType (..), ScalarValue (..))
import Data.Text (Text)
import GHC.Generics (Generic)

---- GQL Deity -------------------------------
data Deity (m :: * -> *) = Deity
  { fullName :: () -> m Text,
    power :: () -> m (Maybe Power)
  }
  deriving (Generic, GQLType)

---- GQL City -------------------------------
data City
  = Athens
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic)

instance GQLType City where
  type KIND City = ENUM

---- GQL Power -------------------------------
data Power
  = Power Int Int

instance GQLScalar Power where
  parseValue _ = pure (Power 0 0)
  serialize (Power x y) = Int (x + y)

instance GQLType Power where
  type KIND Power = SCALAR

---- GQL Creature -------------------------------
data Creature (m :: * -> *) = Creature
  { creatureName :: () -> m Text,
    realm :: () -> m City,
    immortality :: () -> m Bool
  }
  deriving (Generic, GQLType)

---- GQL Character -------------------------------
data Character (m :: * -> *)
  = CharacterCreature (Creature m)
  | CharacterDeity (Deity m)
  deriving (Generic, GQLType)
