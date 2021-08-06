{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Inference.TaggedArgumentsFail
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
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
  {a2 :: Text}
  deriving (Show, Generic, GQLType)

newtype Query (m :: * -> *) = Query
  { field1 :: A -> B -> m Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { field1 = \a b -> pure $ pack $ show (a, b)
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
