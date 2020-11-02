{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Subscriptions
  ( webSocketsApp,
    httpPubApp,
    PubApp (..),
    SubApp (..),
    Event (..),
  )
where

import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
  )
import Data.Morpheus.Core
  ( App,
    runApp,
  )
import Data.Morpheus.Subscriptions.Event
  ( Event (..),
    runEvents,
  )
import Data.Morpheus.Subscriptions.Internal
  ( ApiContext (..),
    Input (..),
    connectionThread,
    initDefaultStore,
    publishEventWith,
    runStreamHTTP,
    streamApp,
  )
import Data.Morpheus.Subscriptions.WebSockets
  ( webSocketsWrapper,
  )
import Data.Morpheus.Types.IO (MapAPI (..))
import Network.WebSockets
  ( ServerApp,
  )
import Relude

httpPubApp ::
  ( PubApp e,
    MapAPI a b,
    MonadIO m
  ) =>
  [e -> m ()] ->
  App e m ->
  a ->
  m b
httpPubApp = runPubApp

-- | Wai WebSocket Server App for GraphQL subscriptions
webSocketsApp ::
  ( MonadUnliftIO m,
    MonadIO m,
    SubApp ServerApp e
  ) =>
  App e m ->
  m (ServerApp, e -> m ())
webSocketsApp = runSubApp

class SubApp app e where
  runSubApp :: (MonadIO m, MonadUnliftIO m) => App e m -> m (app, e -> m ())

class PubApp e where
  runPubApp :: (MonadIO m, MapAPI a b) => [e -> m ()] -> App e m -> a -> m b

instance (Show ch, Eq ch, Hashable ch) => SubApp ServerApp (Event ch con) where
  runSubApp app = do
    store <- initDefaultStore
    wsApp <- webSocketsWrapper store (connectionThread app)
    pure
      ( wsApp,
        publishEventWith store
      )

instance (Show ch, Eq ch, Hashable ch) => PubApp (Event ch con) where
  runPubApp [] app = runApp app
  runPubApp callbacks app =
    mapAPI $
      runStreamHTTP PubContext {eventPublisher = runEvents callbacks}
        . streamApp app
        . Request

instance SubApp () () where
  runSubApp _ = pure ((), const $ pure ())

instance PubApp () where
  runPubApp _ = runApp
