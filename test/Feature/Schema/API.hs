{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.Schema.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (KIND, OBJECT)
import           Data.Morpheus.Types        (GQLQuery, GQLRootResolver (..), GQLType (..))
import           Data.Text                  (Text)
import qualified Feature.Schema.A2          as A2 (A (..))
import           GHC.Generics               (Generic)

type instance KIND A = OBJECT

data A = A
  { aText :: Text
  , aInt  :: Int
  } deriving (Generic, GQLType)

data Query = Query
  { a1 :: A
  , a2 :: A2.A
  } deriving (Generic, GQLQuery)

api :: ByteString -> IO ByteString
api =
  interpreter
    GQLRootResolver {queryResolver = Query {a1 = A "" 0, a2 = A2.A 0}, mutationResolver = (), subscriptionResolver = ()}
