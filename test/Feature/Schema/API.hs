{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.Schema.API
  ( api
  ) where

import           Data.Morpheus       (interpreter)
import           Data.Morpheus.Kind  (OBJECT)
import           Data.Morpheus.Types (GQLRequest, GQLResponse, GQLRootResolver (..), GQLType (..))
import           Data.Text           (Text)
import qualified Feature.Schema.A2   as A2 (A (..))
import           GHC.Generics        (Generic)

data A = A
  { aText :: Text
  , aInt  :: Int
  } deriving (Generic)

instance GQLType A where
  type KIND A = OBJECT

data Query = Query
  { a1 :: A
  , a2 :: A2.A
  } deriving (Generic)

rootResolver :: GQLRootResolver IO () () Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {a1 = A "" 0, a2 = A2.A 0}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
