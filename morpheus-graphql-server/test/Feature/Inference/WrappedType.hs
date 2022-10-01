{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Feature.Inference.WrappedType
  ( api,
  )
where

import Data.Morpheus (interpreter)
import Data.Morpheus.Subscriptions (Event)
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    RootResolver (..),
    SubscriptionField,
    constRes,
    subscribe,
  )
import Relude

data Wrapped a b = Wrapped
  { fieldA :: a,
    fieldB :: b
  }
  deriving (Generic, GQLType)

data WA m = WA
  { aText :: m Text,
    aInt :: Int
  }
  deriving (Generic, GQLType)

type Wrapped1 = Wrapped Int Int

type Wrapped2 = Wrapped (Wrapped Text Int) Text

data Query m = Query
  { a1 :: WA m,
    a2 :: Maybe Wrapped1,
    a3 :: Maybe Wrapped2
  }
  deriving (Generic, GQLType)

data Mutation m = Mutation
  { mut1 :: Maybe (WA m),
    mut2 :: Maybe Wrapped1,
    mut3 :: Maybe Wrapped2
  }
  deriving (Generic, GQLType)

data Channel
  = Channel
  deriving (Show, Eq)

type EVENT = Event Channel ()

data Subscription (m :: Type -> Type) = Subscription
  { sub1 :: SubscriptionField (m (Maybe (WA m))),
    sub2 :: SubscriptionField (m (Maybe Wrapped1)),
    sub3 :: SubscriptionField (m (Maybe Wrapped2))
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO EVENT Query Mutation Subscription
rootResolver =
  RootResolver
    { queryResolver = Query {a1 = WA {aText = pure "test1", aInt = 0}, a2 = Nothing, a3 = Nothing},
      mutationResolver = Mutation {mut1 = Nothing, mut2 = Nothing, mut3 = Nothing},
      subscriptionResolver =
        Subscription
          { sub1 = subscribe Channel (pure $ constRes Nothing),
            sub2 = subscribe Channel (pure $ constRes Nothing),
            sub3 = subscribe Channel (pure $ constRes Nothing)
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
