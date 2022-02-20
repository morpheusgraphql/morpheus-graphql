{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Input.Collections
  ( api,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( Arg (..),
    GQLRequest,
    GQLResponse,
    GQLType,
    RootResolver (..),
    Undefined (..),
  )
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- query
testRes :: Applicative m => Arg "value" a -> m a
testRes = pure . argValue

type Coll m a = Arg "value" a -> m a

data Product = Product Text Int Bool Float
  deriving (Generic, GQLType)

-- resolver
data Query m = Query
  { testSet :: Coll m (Set Int),
    testNonEmpty :: Coll m (NonEmpty Int),
    tesSeq :: Coll m (Seq Int),
    testVector :: Coll m (Vector Int),
    testProduct :: Coll m Product,
    testTuple :: Coll m (Text, Int)
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { testSet = testRes,
            testNonEmpty = testRes,
            tesSeq = testRes,
            testVector = testRes,
            testTuple = testRes
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
