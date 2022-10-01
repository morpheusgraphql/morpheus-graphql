{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Inference.TaggedArguments
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( Arg (..),
    GQLRequest,
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

data A = A
  { a1 :: Text,
    a2 :: Int
  }
  deriving
    ( Show,
      Generic,
      GQLType
    )

newtype B = B
  {b1 :: Text}
  deriving
    ( Show,
      Generic,
      GQLType
    )

newtype C = C
  {c1 :: Int}
  deriving
    ( Show,
      Generic,
      GQLType
    )

data Query (m :: Type -> Type) = Query
  { field1 :: A -> B -> C -> m Text,
    field2 :: A -> Arg "b1" Text -> Arg "c1" Int -> m Text
  }
  deriving
    ( Generic,
      GQLType
    )

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver =
        Query
          { field1 = \a b c -> pure $ pack $ show (a, b, c),
            field2 = \a b c -> pure $ pack $ show (a, b, c)
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root
