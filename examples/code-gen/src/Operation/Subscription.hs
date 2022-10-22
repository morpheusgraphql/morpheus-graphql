{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Operation.Subscription where

import Data.Data (Typeable)
import Data.Morpheus ()
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Globals.GQLScalars

data Query m = Query
  { deity :: DeityArgs -> m (Deity m),
    character :: CharacterArgs -> m (Character m),
    hero :: m (Human m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE

data DeityArgs = DeityArgs
  { name :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    mythology :: Maybe Text
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

data Mutation m = Mutation
  { createDeity :: CreateDeityArgs -> m (Deity m),
    createCharacter :: CreateCharacterArgs -> m (Character m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Mutation m) where
  type KIND (Mutation m) = TYPE

data CreateDeityArgs = CreateDeityArgs
  { deityName :: Maybe [Maybe [Maybe [[Maybe [Text]]]]],
    deityMythology :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType CreateDeityArgs where
  type KIND CreateDeityArgs = TYPE

data CreateCharacterArgs = CreateCharacterArgs
  { charRealm :: Realm,
    charMutID :: Text
  }
  deriving (Generic, Show)

instance GQLType CreateCharacterArgs where
  type KIND CreateCharacterArgs = TYPE

data Subscription m = Subscription
  { newDeity :: m (Deity m),
    newCharacter :: m (Character m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Subscription m) where
  type KIND (Subscription m) = TYPE

data Character m
  = CharacterCreature (Creature m)
  | CharacterDeity (Deity m)
  | CharacterHuman (Human m)
  deriving (Generic)

instance (Typeable m) => GQLType (Character m) where
  type KIND (Character m) = TYPE

data Deity m = Deity
  { fullName :: m Text,
    power :: m ScalarPower
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Deity m) where
  type KIND (Deity m) = TYPE

data Creature m = Creature
  { creatureName :: m Text,
    realm :: m City
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Creature m) where
  type KIND (Creature m) = TYPE
  directives _ =
    fieldDirective "creatureName" Describe {text = "\n  simple field description\n  "}

data Human m = Human
  { humanName :: m Text,
    profession :: m (Maybe Text)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Human m) where
  type KIND (Human m) = TYPE

data Realm = Realm
  { owner :: Text,
    place :: Maybe Int
  }
  deriving (Generic, Show)

instance GQLType Realm where
  type KIND Realm = TYPE

data City
  = Athens
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic, Show)

instance GQLType City where
  type KIND City = TYPE
