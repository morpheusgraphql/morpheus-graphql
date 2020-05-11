{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Feature.Input.Enum.API
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Kind (ENUM)
import Data.Morpheus.Types (GQLRequest, GQLResponse, GQLRootResolver (..), GQLType (..), Undefined (..))
import GHC.Generics (Generic)

data TwoCon
  = LA
  | LB
  deriving (Show, Generic)

data ThreeCon
  = T1
  | T2
  | T3
  deriving (Show, Generic)

data Level
  = L0
  | L1
  | L2
  | L3
  | L4
  | L5
  | L6
  deriving (Show, Generic)

-- types & args
newtype TestArgs a = TestArgs
  { level :: a
  }
  deriving (Generic, Show, GQLType)

instance GQLType Level where
  type KIND Level = ENUM

instance GQLType TwoCon where
  type KIND TwoCon = ENUM

instance GQLType ThreeCon where
  type KIND ThreeCon = ENUM

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

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = Query {test = testRes, test2 = testRes, test3 = testRes},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
