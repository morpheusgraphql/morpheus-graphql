{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Wrappers.API
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
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- query
testRes :: Applicative m => Arg "value" a -> m a
testRes = pure . argValue

type Coll m a = Arg "value" a -> m a

-- resolver
data Query m = Query
  { testSet :: Coll m (Set Int),
    testNonEmpty :: Coll m (NonEmpty Int),
    tesSeq :: Coll m (Seq Int),
    testVector :: Coll m (Vector Int)
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
            testVector = testRes
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver