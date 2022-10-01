{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Inference.TypeGuards
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
    TypeGuard (..),
    Undefined,
    defaultRootResolver,
  )
import Data.Text
  ( Text,
  )
import GHC.Generics (Generic)

data Character = Hydra
  { name :: Text,
    age :: Int
  }
  deriving (Show, Generic, GQLType)

data Deity (m :: Type -> Type) = Deity
  { name :: Text,
    age :: Int,
    power :: Text
  }
  deriving (Generic, GQLType)

data Implements (m :: Type -> Type)
  = ImplementsDeity (Deity m)
  | Creature {name :: Text, age :: Int}
  deriving (Generic, GQLType)

type Characters m = TypeGuard Character (Implements m)

newtype Query (m :: Type -> Type) = Query
  { characters :: [Characters m]
  }
  deriving (Generic, GQLType)

resolveCharacters :: [Characters m]
resolveCharacters =
  ResolveType
    <$> [ ImplementsDeity
            ( Deity
                { name = "Morpheus",
                  age = 2000,
                  power = "Shapeshift"
                }
            ),
          Creature
            { name = "Lamia",
              age = 205
            }
        ]

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query {characters = resolveCharacters}
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
