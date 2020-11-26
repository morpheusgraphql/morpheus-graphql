{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Input.Enum.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Types (GQLRequest, GQLResponse, GQLType (..), RootResolver (..), Undefined (..))
import GHC.Generics (Generic)

data TwoCon
  = LA
  | LB
  deriving (Show, Generic, GQLType)

data ThreeCon
  = T1
  | T2
  | T3
  deriving (Show, Generic, GQLType)

data Level
  = L0
  | L1
  | L2
  | L3
  | L4
  | L5
  | L6
  deriving (Show, Generic, GQLType)

-- types & args
newtype TestArgs a = TestArgs
  { level :: a
  }
  deriving (Generic, Show, GQLType)

-- query
testRes :: Applicative m => TestArgs a -> m a
testRes TestArgs {level} = pure level

-- resolver
data Query m = Query
  { test :: TestArgs Level -> m Level,
    test2 :: TestArgs TwoCon -> m TwoCon,
    test3 :: TestArgs ThreeCon -> m ThreeCon
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {test = testRes, test2 = testRes, test3 = testRes},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
