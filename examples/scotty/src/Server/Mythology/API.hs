{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Mythology.API
  ( app,
  )
where

import Data.Morpheus
  ( App,
    deriveApp,
  )
import Data.Morpheus.Types
  ( GQLType,
    ResolverQ,
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    liftEither,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Server.Mythology.Character
  ( Deity,
    Human,
    PersonGuard,
    dbDeity,
    resolvePersons,
    someDeity,
    someHuman,
  )
import Server.Mythology.Place (City)

data Character m
  = CharacterHuman (Human m) -- Only <tyconName><conName> should generate direct link
  | CharacterDeity Deity -- Only <tyconName><conName> should generate direct link
  -- RECORDS
  | Creature {name :: Text, age :: Int}
  | BoxedDeity {boxedDeity :: Deity}
  | SomeScalarRecord {scalar :: Text}
  | --- Types
    SomeDeity Deity
  | SomeScalar Int
  | SomeMutli Int Text
  | --- ENUMS
    Zeus
  | Cronus
  deriving (Generic, GQLType)

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    character :: [Character m],
    persons :: [PersonGuard m]
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text, -- Required Argument
    bornPlace :: Maybe City -- Optional Argument
  }
  deriving (Generic, GQLType)

resolveDeity :: DeityArgs -> ResolverQ e IO Deity
resolveDeity DeityArgs {name, bornPlace} =
  liftEither $ dbDeity name bornPlace

resolveCharacter :: (Applicative m) => [Character m]
resolveCharacter =
  [ CharacterHuman someHuman,
    CharacterDeity someDeity,
    Creature {name = "Lamia", age = 205},
    BoxedDeity {boxedDeity = someDeity},
    SomeScalarRecord {scalar = "Some Text"},
    ---
    SomeDeity someDeity,
    SomeScalar 12,
    SomeMutli 21 "some text",
    Zeus,
    Cronus
  ]

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver =
        Query
          { deity = resolveDeity,
            character = resolveCharacter,
            persons = resolvePersons
          }
    }

app :: App () IO
app = deriveApp rootResolver
