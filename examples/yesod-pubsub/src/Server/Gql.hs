{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Server.Gql (gqlApi, rootResolver) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Hashable (Hashable)
import qualified Data.Maybe as Maybe
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import qualified Data.Morpheus.Kind as MT
import Data.Morpheus.Subscriptions (Event (..))
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    KIND,
    ResolverM,
    ResolverQ,
    ResolverS,
    RootResolver (..),
    SubscriptionField,
    Undefined (..),
    lift,
    liftEither,
    publish,
    subscribe,
  )
import qualified Data.Morpheus.Types as MT
import qualified Data.Morpheus.Types.Internal.AST as MT
import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import GHC.Generics
import Server.ServerState (Channel (..), Content (..), MEvent, ServerState)
import qualified Server.ServerState as ServerState

newtype Query m
  = Query {queryCounter :: m Int}
  deriving (Generic, GQLType)

newtype FunctionSearchArgs
  = FunctionSearchArgs {fn :: Text}
  deriving (Generic, GQLType)

data QueryCounterArgs
  = QueryCounterArgs
  deriving (Generic, GQLType)

emitNewChannelResults :: ServerState -> Int -> ResolverM MEvent IO ()
emitNewChannelResults serverState results = do
  lift $ putStrLn "emitting to subscription!"
  publish
    [ Event
        { channels = [ChannelCounter],
          content = Count results
        }
    ]

resolveQuCounter :: ServerState -> ResolverQ e IO Int
resolveQuCounter serverState = do
  results <- lift $ ServerState.readResults serverState
  liftEither . return $ Right results

newtype Mutation m
  = Mutation {updateCounter :: UpdateCounter -> m Int}
  deriving (Generic, GQLType)

newtype UpdateCounter
  = UpdateCounter {updateCounter_newVal :: Int}
  deriving (Generic, GQLType)

mutUpdateCounter :: ServerState -> UpdateCounter -> ResolverM MEvent IO Int
mutUpdateCounter ss args@UpdateCounter {updateCounter_newVal} = do
  lift $ ServerState.saveResults ss updateCounter_newVal
  emitNewChannelResults ss updateCounter_newVal
  liftEither . return $ Right updateCounter_newVal

--- Subscriptions
newtype Subscription (m :: * -> *) = Subscription
  { subCounter :: SubscriptionField (m Int)
  }
  deriving (Generic, GQLType)

rootResolver ::
  ServerState ->
  RootResolver IO MEvent Query Mutation Subscription
rootResolver ss =
  MT.defaultRootResolver
    { queryResolver =
        Query
          { queryCounter = resolveQuCounter ss
          },
      mutationResolver =
        Mutation
          { updateCounter = mutUpdateCounter ss
          },
      subscriptionResolver =
        Subscription
          { subCounter = rsubCounter ss
          }
    }

gqlApi :: ServerState -> GQLRequest -> IO GQLResponse
gqlApi serverState = interpreter $ rootResolver serverState

rsubCounter :: ServerState -> SubscriptionField (ResolverS MEvent IO Int)
rsubCounter ss = do
  subscribe ChannelCounter $ do
    pure $ \(Event _ content) -> do
      lift $ ServerState.readResults ss
