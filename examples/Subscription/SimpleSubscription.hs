{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Subscription.SimpleSubscription where

import           Data.Morpheus.Types       (Event (..), GQLRootResolver (..), IOSubRes, SubResolver (..), resolver,
                                            toMutResolver)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Mythology.Character.Deity (Deity (..), dbDeity)


-- TODO: importGQLDocument "examples/Subscription/api.gql"
--
data Channel
  = ChannelA
  | ChannelB

data Content
  = ContentA Int
  | ContentB Text

type MyEvent = Event Channel Content

newtype Query m = Query
  { deity :: () -> m Deity
  } deriving (Generic)

newtype Mutation m = Mutation
  { createDeity :: () -> m Deity
  } deriving (Generic)

newtype Subscription (m ::  * -> * ) = Subscription
  { newDeity :: () -> IOSubRes MyEvent Deity
  } deriving (Generic)

type APIEvent = Event Channel Content

rootResolver :: GQLRootResolver IO APIEvent Query Mutation Subscription
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {deity = const fetchDeity}
    , mutationResolver = return Mutation {createDeity}
    , subscriptionResolver = return Subscription {newDeity}
    }
  where
    fetchDeity = resolver $ dbDeity "" Nothing
    createDeity _args = toMutResolver [Event {channels = [ChannelA], content = ContentA 1}] fetchDeity
    newDeity _args = SubResolver {subChannels = [ChannelA], subResolver}
      where
        subResolver (Event [ChannelA] (ContentA _value)) = resolver $ dbDeity "" Nothing -- resolve New State
        subResolver (Event [ChannelA] (ContentB value))  = resolver $ dbDeity value Nothing -- resolve New State
        subResolver _                                    = fetchDeity -- Resolve Old State
