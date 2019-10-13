{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.InputType.API
  ( api
  ) where

import           Data.Morpheus       (interpreter)
import           Data.Morpheus.Kind  (OBJECT)
import           Data.Morpheus.Types (GQLRequest, GQLResponse, GQLRootResolver (..), GQLType (..), IORes)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

data F1Args = F1Args
  { arg1 :: Text
  , arg2 :: Maybe Int
  } deriving (Generic)

data F2Args = F2Args
  { argList       :: [Text]
  , argNestedList :: [Maybe [[Int]]]
  } deriving (Generic)

data A = A
  { a1 :: F1Args -> IORes Text
  , a2 :: F2Args -> IORes Int
  } deriving (Generic)

instance GQLType A where
  type KIND A = OBJECT

newtype Query = Query
  { q1 :: A
  } deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {q1 = A {a1 = const $ return "a1Test", a2 = const $ return 1}}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
