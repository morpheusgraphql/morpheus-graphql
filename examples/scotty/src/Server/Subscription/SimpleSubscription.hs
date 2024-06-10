{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Subscription.SimpleSubscription where

import Data.Kind (Type)
import Data.Morpheus.Subscriptions (Event (..))
import Data.Morpheus.Types
  ( GQLType (..),
    Resolver,
    RootResolver (..),
    SubscriptionField,
    WithOperation,
    publish,
    subscribe,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Server.Mythology.Character
  ( Deity,
    someDeity,
  )

data Channel
  = ChannelA
  | ChannelB

data Content
  = ContentA Int
  | ContentB Text

newtype Query m = Query
  { deity :: m Deity
  }
  deriving (Generic, GQLType)

newtype Mutation m = Mutation
  { createDeity :: m Deity
  }
  deriving (Generic, GQLType)

newtype Subscription (m :: Type -> Type) = Subscription
  { newDeity :: SubscriptionField (m Deity)
  }
  deriving (Generic, GQLType)

type APIEvent = Event Channel Content

rootResolver :: RootResolver IO APIEvent Query Mutation Subscription
rootResolver =
  RootResolver
    { queryResolver = Query {deity = fetchDeity},
      mutationResolver = Mutation {createDeity},
      subscriptionResolver = Subscription {newDeity}
    }
  where
    createDeity = do
      requireAuthorized
      publish [Event {channels = [ChannelA], content = ContentA 1}]
      pure someDeity
    newDeity = subscribe ChannelA $ do
      requireAuthorized
      pure subResolver
      where
        subResolver (Event [ChannelA] (ContentA _value)) = fetchDeity -- resolve New State
        subResolver (Event [ChannelA] (ContentB _value)) = fetchDeity -- resolve New State
        subResolver _ = fetchDeity -- Resolve Old State
        ---------------------------------------------------------
    fetchDeity :: (Applicative m) => m Deity
    fetchDeity = pure someDeity

requireAuthorized :: (WithOperation o) => Resolver o e IO ()
requireAuthorized = pure ()
