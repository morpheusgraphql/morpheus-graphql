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
import           Data.Morpheus.Kind         (KIND, OBJECT)
import           Data.Morpheus.Types        (EffectM, GQLRootResolver (..), GQLType (..), ResM)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND (WA a) = OBJECT

type instance KIND (Wrapped a b) = OBJECT

data Wrapped a b = Wrapped
  { fieldA :: a
  , fieldB :: b
  } deriving (Generic, GQLType)

data WA m = WA
  { aText :: () -> m Text
  , aInt  :: Int
  } deriving (Generic, GQLType)

data Query = Query
  { a1 :: WA ResM
  , a2 :: Maybe (Wrapped Int Int)
  , a3 :: Maybe (Wrapped (Wrapped Text Int) Text)
  } deriving (Generic)

data Mutation = Mutation
  { mut1 :: Maybe (WA EffectM)
  , mut2 :: Maybe (Wrapped Int Int)
  , mut3 :: Maybe (Wrapped (Wrapped Text Int) Text)
  } deriving (Generic)

data Subscription = Subscription
  { sub1 :: Maybe (WA EffectM)
  , sub2 :: Maybe (Wrapped Int Int)
  , sub3 :: Maybe (Wrapped (Wrapped Text Int) Text)
  } deriving (Generic)

api :: ByteString -> IO ByteString
api =
  interpreter
    GQLRootResolver
      { queryResolver = Query {a1 = WA {aText = const $ pure "test1", aInt = 0}, a2 = Nothing, a3 = Nothing}
      , mutationResolver = Mutation {mut1 = Nothing, mut2 = Nothing, mut3 = Nothing}
      , subscriptionResolver = Subscription {sub1 = Nothing, sub2 = Nothing, sub3 = Nothing}
      }
