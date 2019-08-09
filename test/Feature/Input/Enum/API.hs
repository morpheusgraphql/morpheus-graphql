{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Feature.Input.Enum.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (ENUM)
import           Data.Morpheus.Types        (GQLRootResolver (..), GQLType (..), IORes)
import           GHC.Generics               (Generic)

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
  } deriving (Generic, Show)

instance GQLType Level where
  type KIND Level = ENUM

instance GQLType TwoCon where
  type KIND TwoCon = ENUM

instance GQLType ThreeCon where
  type KIND ThreeCon = ENUM

-- query
testRes :: TestArgs a -> IORes a
testRes TestArgs {level} = return level

-- resolver
data Query = Query
  { test  :: TestArgs Level -> IORes Level
  , test2 :: TestArgs TwoCon -> IORes TwoCon
  , test3 :: TestArgs ThreeCon -> IORes ThreeCon
  } deriving (Generic)

rootResolver :: GQLRootResolver IO () () Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {test = testRes, test2 = testRes, test3 = testRes}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

api :: ByteString -> IO ByteString
api = interpreter rootResolver
