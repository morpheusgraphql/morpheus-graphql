{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.SimpleSubscription where

import           Data.Morpheus.Types       (Event (..), GQLRootResolver (..), IOMutRes, IORes, IOSubRes, resolver,
                                            toMutResolver)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Mythology.Character.Deity (Deity (..), dbDeity)

data Channel
  = ChannelA
  | ChannelB

data Content
  = ContentA Int
  | ContentB Text

newtype Query = Query
  { deity :: () -> IORes Deity
  } deriving (Generic)

newtype Mutation = Mutation
  { createDeity :: () -> IOMutRes Channel Content Deity
  } deriving (Generic)

newtype Subscription = Subscription
  { newDeity :: () -> IOSubRes Channel Content Deity
  } deriving (Generic)

rootResolver :: GQLRootResolver IO Channel Content Query Mutation Subscription
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {deity = const fetchDeity}
    , mutationResolver = return Mutation {createDeity}
    , subscriptionResolver = return Subscription {newDeity}
    }
  where
    fetchDeity = resolver $ dbDeity "" Nothing
    createDeity _args = toMutResolver [Event {channels = [ChannelA], content = ContentA 1}] fetchDeity
    newDeity _args = Event {channels = [ChannelA], content}
      where
        content (Event [ChannelA] (ContentA _value)) = resolver $ dbDeity "" Nothing -- resolve New State
        content (Event [ChannelA] (ContentB value))  = resolver $ dbDeity value Nothing -- resolve New State
        content _                                    = fetchDeity -- Resolve Old State
