{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Subscription
  ( webSocketsApp,
    runPubApp,
    SubscriptionApp (..),
    Event (..),
  )
where

import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
    withRunInIO,
  )
import Data.Morpheus.Core
  ( App,
    runApp,
  )
import Data.Morpheus.Subscription.Event
  ( Event (..),
  )
import Data.Morpheus.Subscription.Internal
  ( ApiContext (..),
    Input (..),
    SUB,
    Store (..),
    SubscriptionApp (..),
    acceptApolloRequest,
    connectionThread,
    initDefaultStore,
    publishEventWith,
    runStreamHTTP,
    streamApp,
  )
import Data.Morpheus.Types.IO (MapAPI (..))
import Network.WebSockets
  ( Connection,
    ServerApp,
    receiveData,
    sendTextData,
  )
import qualified Network.WebSockets as WS
import Relude

-- support old version of Websockets
pingThread :: Connection -> IO () -> IO ()

#if MIN_VERSION_websockets(0,12,6)
pingThread connection = WS.withPingThread connection 30 (return ())
#else
pingThread connection = (WS.forkPingThread connection 30 >>)
#endif

defaultWSScope :: MonadIO m => Store e m -> Connection -> ApiContext SUB e m
defaultWSScope Store {writeStore} connection =
  SubContext
    { listener = liftIO (receiveData connection),
      callback = liftIO . sendTextData connection,
      updateStore = writeStore
    }

runPubApp ::
  SubscriptionApp e =>
  ( MonadIO m,
    MapAPI a b
  ) =>
  [e -> m ()] ->
  App e m ->
  a ->
  m b
runPubApp [] app = runApp app
runPubApp callbacks app =
  mapAPI $
    runStreamHTTP PubContext {eventPublisher}
      . streamApp app
      . Request
  where
    eventPublisher e = traverse_ (e &) callbacks

-- | Wai WebSocket Server App for GraphQL subscriptions
webSocketsApp ::
  ( MonadUnliftIO m,
    Eq channel,
    Hashable channel,
    Show channel
  ) =>
  App (Event channel cont) m ->
  m (ServerApp, Event channel cont -> m ())
webSocketsApp app =
  do
    store <- initDefaultStore
    wsApp <- webSocketsWrapper store (connectionThread app)
    pure
      ( wsApp,
        publishEventWith store
      )

webSocketsWrapper ::
  (MonadUnliftIO m, MonadIO m) =>
  Store e m ->
  (ApiContext SUB e m -> m ()) ->
  m ServerApp
webSocketsWrapper store handler =
  withRunInIO $
    \runIO ->
      pure $
        \pending -> do
          conn <- acceptApolloRequest pending
          pingThread
            conn
            $ runIO (handler (defaultWSScope store conn))
