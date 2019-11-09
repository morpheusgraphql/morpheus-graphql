{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Subscription.SimpleSubscription where

import           Data.Morpheus.Types            ( Event(..)
                                                , Resolver(..)
                                                , GQLRootResolver(..)
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Server.Mythology.Character.Deity
                                                ( Deity(..) )
import           Server.Mythology.Place.Places  ( Realm(..) )

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
  { newDeity :: () -> m  Deity
  } deriving (Generic)

type APIEvent = Event Channel Content

rootResolver :: GQLRootResolver IO APIEvent Query Mutation Subscription
rootResolver = GQLRootResolver
  { queryResolver        = Query { deity = const fetchDeity }
  , mutationResolver     = Mutation { createDeity }
  , subscriptionResolver = Subscription { newDeity }
  }
 where
    -- TODO: resolver $ dbDeity "" Nothing
  createDeity _args = MutResolver $ pure
    ( [Event { channels = [ChannelA], content = ContentA 1 }]
    , Deity { fullName = "", power = Nothing, realm = Sky }
    )
  newDeity _args = SubResolver [ChannelA] subResolver
   where
    subResolver (Event [ChannelA] (ContentA _value)) = fetchDeity  -- resolve New State
    subResolver (Event [ChannelA] (ContentB _value)) = fetchDeity   -- resolve New State
    subResolver _ = fetchDeity -- Resolve Old State
  ---------------------------------------------------------
  fetchDeity = pure Deity { fullName = "", power = Nothing, realm = Sky }
