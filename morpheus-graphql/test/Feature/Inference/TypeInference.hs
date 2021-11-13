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

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    Undefined (..),
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

data Deity (m :: * -> *) = Deity
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

data Character (m :: * -> *)
  = CharacterDeity (Deity m) -- Only <tycon name><type ref name> should generate direct link
  | Creature {creatureName :: Text, creatureAge :: Int}
  | BoxedDeity {boxedDeity :: Deity m}
  | ScalarRecord {scalarText :: Text}
  | CharacterAge Int
  | SomeDeity (Deity m)
  | SomeMutli Int Text
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
    SomeMutli 21 "some text",
    Zeus,
    Cronus
  ]

newtype MonsterArgs = MonsterArgs
  { monster :: Monster
  }
  deriving (Show, Generic, GQLType)

data Query (m :: * -> *) = Query
  { deity :: Deity m,
    character :: [Character m],
    showMonster :: MonsterArgs -> m Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { deity = deityRes,
            character = resolveCharacter,
            showMonster
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    showMonster MonsterArgs {monster} = pure (pack $ show monster)

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
