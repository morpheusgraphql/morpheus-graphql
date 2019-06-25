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
import           Data.Morpheus.Kind         (KIND, MUTATION, OBJECT, QUERY, MUTATION)
import           Data.Morpheus.Types        (GQLMutation, GQLQuery, GQLRootResolver (..), GQLSubscription, GQLType (..),
                                             Resolver)
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

data Mutation = Mutation
  { mut1 :: Maybe (WA (MUTATION IO Text))
  , mut2 :: Maybe (Wrapped Int Int)
  , mut3 :: Maybe (Wrapped (Wrapped Text Int) Text)
  } deriving (Generic, GQLMutation)

data Subscription = Subscription
  { sub1 :: Maybe (WA (MUTATION IO Text)) -- TODO: test Subscription when it implemented
  , sub2 :: Maybe (Wrapped Int Int)
  , sub3 :: Maybe (Wrapped (Wrapped Text Int) Text)
  } deriving (Generic, GQLSubscription)

api :: ByteString -> IO ByteString
api =
  interpreter
    GQLRootResolver
      { queryResolver = Query {a1 = WA {aText = pure "test1", aInt = 0}, a2 = Nothing, a3 = Nothing}
      , mutationResolver = Mutation {mut1 = Nothing, mut2 = Nothing, mut3 = Nothing}
      , subscriptionResolver = Subscription {sub1 = Nothing, sub2 = Nothing, sub3 = Nothing}
      }
