{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
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

deityRes :: Deity m
deityRes = Deity {name = "Morpheus", power = "Shapeshift"}

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
  [ Guard (ImplementsDeity deityRes),
    Guard (Creature {name = "Lamia", age = 205})
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
