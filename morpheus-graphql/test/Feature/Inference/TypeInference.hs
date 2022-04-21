{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Inference.TypeInference
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
  )
import Data.Text
  ( Text,
    pack,
  )
import GHC.Generics (Generic)

data Power
  = Thunderbolts
  | Shapeshift
  | Hurricanes
  deriving (Generic, GQLType)

data Deity (m :: Type -> Type) = Deity
  { name :: Text,
    power :: Power
  }
  deriving (Generic, GQLType)

deityRes :: Deity m
deityRes = Deity {name = "Morpheus", power = Shapeshift}

data Hydra = Hydra
  { name :: Text,
    age :: Int
  }
  deriving (Show, Generic, GQLType)

data Monster
  = MonsterHydra Hydra
  | Cerberus {name :: Text}
  | UnidentifiedMonster
  deriving (Show, Generic, GQLType)

data Character (m :: Type -> Type)
  = CharacterDeity (Deity m) -- Only <tyCon name><type ref name> should generate direct link
  | Creature {creatureName :: Text, creatureAge :: Int}
  | BoxedDeity {boxedDeity :: Deity m}
  | ScalarRecord {scalarText :: Text}
  | CharacterAge Int
  | SomeDeity (Deity m)
  | SomeCompound Int Text
  | Zeus
  | Cronus
  deriving (Generic, GQLType)

resolveCharacter :: [Character m]
resolveCharacter =
  [ CharacterDeity deityRes,
    Creature {creatureName = "Lamia", creatureAge = 205},
    BoxedDeity {boxedDeity = deityRes},
    ScalarRecord {scalarText = "Some Text"},
    SomeDeity deityRes,
    CharacterAge 12,
    SomeCompound 21 "some text",
    Zeus,
    Cronus
  ]

newtype MonsterArgs = MonsterArgs
  { monster :: Monster
  }
  deriving (Show, Generic, GQLType)

data Query (m :: Type -> Type) = Query
  { deity :: Deity m,
    character :: [Character m],
    showMonster :: MonsterArgs -> m Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver =
        Query
          { deity = deityRes,
            character = resolveCharacter,
            showMonster
          }
    }
  where
    showMonster MonsterArgs {monster} = pure (pack $ show monster)

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
