{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Feature.Input.Scalar.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLRootResolver (..),
    GQLType (..),
    Undefined (..),
  )
import GHC.Generics (Generic)

-- types & args
newtype Arg a = Arg
  { value :: a
  }
  deriving (Generic, Show, GQLType)

-- query
testRes :: Applicative m => Arg a -> m a
testRes Arg {value} = pure value

-- resolver
data Query m = Query
  { testFloat :: Arg Float -> m Float,
    testInt :: Arg Int -> m Int
  }
  deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = Query {testFloat = testRes, testInt = testRes},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
