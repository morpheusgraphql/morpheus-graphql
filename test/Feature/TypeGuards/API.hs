{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.TypeGuards.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    Guard (..),
    RootResolver (..),
    Undefined (..),
  )
import Data.Text
  ( Text,
    pack,
  )
import GHC.Generics (Generic)

data Character = Hydra
  { name :: Text,
    age :: Int
  }
  deriving (Show, Generic, GQLType)

data Deity (m :: * -> *) = Deity
  { name :: Text,
    age :: Int,
    power :: Text
  }
  deriving (Generic, GQLType)

data ImplementsCharacter (m :: * -> *)
  = ImplementsDeity (Deity m)
  | Creature {name :: Text, age :: Int}
  deriving (Generic, GQLType)

type Characters m = Guard "Character" Character (ImplementsCharacter m)

newtype Query (m :: * -> *) = Query
  { characters :: [Characters m]
  }
  deriving (Generic, GQLType)

resolveCharacters :: [Characters m]
resolveCharacters =
  Guard
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
  RootResolver
    { queryResolver =
        Query
          { characters = resolveCharacters
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
