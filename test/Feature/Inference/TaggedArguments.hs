{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Inference.TaggedArguments
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( Arg (..),
    GQLRequest,
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

data A = A
  { a1 :: Text,
    a2 :: Int
  }
  deriving (Show, Generic, GQLType)

newtype B = B
  {b1 :: Text}
  deriving (Show, Generic, GQLType)

newtype C = C
  {c1 :: Int}
  deriving (Show, Generic, GQLType)

data Query (m :: * -> *) = Query
  { field1 :: A -> B -> C -> m Text,
    field2 :: A -> Arg "b1" Text -> Arg "c1" Int -> m Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { field1 = \a b c -> pure $ pack $ show (a, b, c),
            field2 = \a b c -> pure $ pack $ show (a, b, c)
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
