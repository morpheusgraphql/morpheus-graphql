{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Collision.NameCollision
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
import Data.Text (Text)
import qualified Feature.Collision.NameCollisionHelper as A2 (A (..))
import GHC.Generics (Generic)

data A = A
  { aText :: Text,
    aInt :: Int
  }
  deriving (Generic, GQLType)

data Query (m :: Type -> Type) = Query
  { a1 :: A,
    a2 :: A2.A
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query {a1 = A "" 0, a2 = A2.A 0}
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
