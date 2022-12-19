{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Server.Gql
  (gqlApi, rootResolver) where

import qualified Debug.Trace as Debug
import           GHC.Generics
import qualified Data.Maybe as Maybe
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Hashable (Hashable)
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Document (importGQLDocument)
import           Data.Morpheus.Subscriptions (Event(..))
import           Data.Morpheus.Types
  ( GQLRequest, GQLType(..), GQLResponse, Undefined(..)
  , ResolverQ, ResolverM, ResolverS, RootResolver(..)
  , KIND, SubscriptionField, subscribe, lift, liftEither, publish
  )
import qualified Data.Morpheus.Types as MT
import qualified Data.Morpheus.Kind as MT
import qualified Data.Morpheus.Types.Internal.AST as MT
import           Data.Text (Text)
import qualified Data.Text as T

import           Server.ServerState (ServerState, Channel(..), Content(..), MEvent)
import qualified Server.ServerState as ServerState

data Query m =
  Query
    { queryCounter :: m Int
    }
  deriving (Generic,GQLType)

data FunctionSearchArgs =
  FunctionSearchArgs
    { fn      :: Text
    }
  deriving (Generic,GQLType)

data QueryCounterArgs =
  QueryCounterArgs
  deriving (Generic,GQLType)

emitNewChannelResults :: ServerState -> Int -> ResolverM MEvent IO ()
emitNewChannelResults serverState results = do
  lift $ putStrLn "emitting to subscription!"
  publish
    [ Event
        { channels = [ ChannelCounter ]
        , content = Count results
        }
    ]

resolveQuCounter :: ServerState -> ResolverQ e IO Int
resolveQuCounter serverState= do
  results <- lift $ ServerState.readResults serverState
  liftEither . return $ Right results

data Mutation m =
  Mutation
    { updateCounter :: UpdateCounter -> m Int
    }
  deriving (Generic,GQLType)

data UpdateCounter =
  UpdateCounter { updateCounter_newVal :: Int }
  deriving (Generic,GQLType)

mut_updateCounter :: ServerState -> UpdateCounter -> ResolverM MEvent IO Int
mut_updateCounter ss args@UpdateCounter{updateCounter_newVal} = do
  lift $ ServerState.saveResults ss updateCounter_newVal
  emitNewChannelResults ss updateCounter_newVal
  liftEither . return $ Right updateCounter_newVal

--- Subscriptions
newtype Subscription (m :: * -> * ) =
  Subscription
    { sub_counter :: SubscriptionField (m Int)
    }
  deriving (Generic,GQLType)

rootResolver :: ServerState
             -> RootResolver IO MEvent Query Mutation Subscription
rootResolver ss =
  MT.defaultRootResolver
    { queryResolver =
        Query { queryCounter = resolveQuCounter ss
              }
    , mutationResolver =
        Mutation { updateCounter = mut_updateCounter ss
                 }
    , subscriptionResolver =
        Subscription { sub_counter = rsub_counter ss
                     }
    }

gqlApi :: ServerState -> GQLRequest -> IO GQLResponse
gqlApi serverState = interpreter $ rootResolver serverState

rsub_counter :: ServerState -> SubscriptionField (ResolverS MEvent IO Int)
rsub_counter ss = do
  subscribe ChannelCounter $ do
    pure $ \(Event _ content) -> do
      lift $ ServerState.readResults ss
