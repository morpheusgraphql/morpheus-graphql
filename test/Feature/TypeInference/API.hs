{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Feature.TypeInference.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Kind (INPUT)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLRootResolver (..),
    GQLType (..),
    Undefined (..),
  )
import Data.Text
  ( Text,
    pack,
  )
import GHC.Generics (Generic)

data Power = Thunderbolts | Shapeshift | Hurricanes
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
  deriving (Show, Generic)

instance GQLType Hydra where
  type KIND Hydra = INPUT

data Monster
  = MonsterHydra Hydra
  | Cerberus {name :: Text}
  | UnidentifiedMonster
  deriving (Show, Generic)

instance GQLType Monster where
  type KIND Monster = INPUT

data Character (m :: * -> *)
  = CharacterDeity (Deity m) -- Only <tycon name><type ref name> should generate direct link
        -- RECORDS
  | Creature {creatureName :: Text, creatureAge :: Int}
  | BoxedDeity {boxedDeity :: Deity m}
  | ScalarRecord {scalarText :: Text}
  | --- Types
    CharacterInt Int -- all scalars mus be boxed
      -- Types
  | SomeDeity (Deity m)
  | SomeMutli Int Text
  | --- ENUMS
    Zeus
  | Cronus
  deriving (Generic, GQLType)

newtype MonsterArgs = MonsterArgs
  { monster :: Monster
  }
  deriving (Generic, GQLType)

data Query (m :: * -> *) = Query
  { deity :: Deity m,
    character :: [Character m],
    showMonster :: MonsterArgs -> m Text
  }
  deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = Query {deity = deityRes, character, showMonster},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    showMonster MonsterArgs {monster} = pure (pack $ show monster)
    character :: [Character m]
    character =
      [ CharacterDeity deityRes,
        Creature {creatureName = "Lamia", creatureAge = 205},
        BoxedDeity {boxedDeity = deityRes},
        ScalarRecord {scalarText = "Some Text"},
        ---
        SomeDeity deityRes,
        CharacterInt 12,
        SomeMutli 21 "some text",
        Zeus,
        Cronus
      ]

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
