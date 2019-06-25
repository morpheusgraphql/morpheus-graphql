{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.WrappedTypeName.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (KIND, MUTATION, OBJECT, QUERY)
import           Data.Morpheus.Types        (GQLMutation, GQLQuery, GQLRootResolver (..), GQLType (..), Resolver)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND (WA a) = OBJECT

type instance KIND (Wrapped a b) = OBJECT

data Wrapped a b = Wrapped
  { fieldA :: a
  , fieldB :: b
  } deriving (Generic, GQLType)

data WA m = WA
  { aText :: Resolver m () Text
  , aInt  :: Int
  } deriving (Generic, GQLType)

data Query = Query
  { a1 :: WA (QUERY IO)
  , a2 :: Maybe (Wrapped Int Int)
  , a3 :: Maybe (Wrapped (Wrapped Text Int) Text)
  } deriving (Generic, GQLQuery)

newtype Mutation = Mutation
  { mut1 :: Maybe (WA (MUTATION IO Text))
  } deriving (Generic, GQLMutation)

api :: ByteString -> IO ByteString
api =
  interpreter
    GQLRootResolver
      { queryResolver = Query {a1 = WA (pure "test1") 0, a2 = Nothing, a3 = Nothing}
      , mutationResolver = Mutation {mut1 = Nothing}
      , subscriptionResolver = ()
      }
