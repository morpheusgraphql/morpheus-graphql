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
    dbDeityStory,
    resolvePersons,
    someDeity,
    someHuman,
  )
import Server.Mythology.Place (City)
import Server.MetaCoq.TestMeta
import Server.MetaCoq.TestMeta2

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
    persons :: [PersonGuard m],
    deity_story :: DeityArgs -> m (Prod Global_env Term)
               
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

resolveDeityStory :: DeityArgs -> ResolverQ e IO ( Prod Global_env Term )
resolveDeityStory DeityArgs {name, bornPlace}=
  liftEither $ dbDeityStory name bornPlace

--dbDeity :: Text -> Maybe City -> IO (Either String Deity)


resolveCharacter :: Applicative m => [Character m]
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
    -- Introspector
    --Introspector rec_def_term
  ]

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver =
        Query
          { deity = resolveDeity,
            character = resolveCharacter,
            persons = resolvePersons,
            deity_story = resolveDeityStory
          }
    }

app :: App () IO
app = deriveApp rootResolver
