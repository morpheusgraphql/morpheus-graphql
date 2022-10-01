{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Inference.ObjectAndEnum
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
import GHC.Generics (Generic)

data MyEnum = MyEnum deriving (Generic, GQLType)

newtype MyObject
  = MyObject Int
  deriving (Generic, GQLType)

data Query (m :: Type -> Type) = Query
  { enum :: MyEnum,
    object :: MyObject
  }
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver =
        Query
          { enum = MyEnum,
            object = MyObject 0
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root
