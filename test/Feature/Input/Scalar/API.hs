{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Input.Scalar.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType,
    RootResolver (..),
    Undefined (..),
  )
import Data.Text (Text)
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
  { testFloat :: Arg Double -> m Double,
    testInt :: Arg Int -> m Int,
    testString :: Arg Text -> m Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { testFloat = testRes,
            testInt = testRes,
            testString = testRes
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
