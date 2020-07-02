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
module Data.Morpheus.Server
  ( webSocketsApp,
    httpPubApp,
    subscriptionApp,
    ServerConstraint,
    httpPlayground,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
    withRunInIO,
  )
-- MORPHEUS
import Data.Morpheus.Server.Playground
  ( httpPlayground,
  )
import Data.Morpheus.Types.IO (MapAPI (..))
import Data.Morpheus.Types.Internal.Resolving
  ( Event,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( HTTP,
    Input (..),
    Scope (..),
    Store (..),
    Stream,
    WS,
    acceptApolloRequest,
    connectionThread,
    initDefaultStore,
    publishEventWith,
    runStreamHTTP,
  )
import Network.WebSockets
  ( Connection,
    ServerApp,
    receiveData,
    sendTextData,
  )
import qualified Network.WebSockets as WS

type ServerConstraint e m =
  ( MonadIO m,
    MonadUnliftIO m
  )

-- support old version of Websockets
pingThread :: Connection -> IO () -> IO ()

#if MIN_VERSION_websockets(0,12,6)
pingThread connection = WS.withPingThread connection 30 (return ())
#else
pingThread connection = (WS.forkPingThread connection 30 >>)
#endif

defaultWSScope :: MonadIO m => Store e m -> Connection -> Scope WS e m
defaultWSScope Store {writeStore} connection =
  ScopeWS
    { listener = liftIO (receiveData connection),
      callback = liftIO . sendTextData connection,
      update = writeStore
    }

httpPubApp ::
  ( MonadIO m,
    MapAPI a b
  ) =>
  (Input HTTP -> Stream HTTP e m) ->
  (e -> m ()) ->
  a ->
  m b
httpPubApp api httpCallback =
  mapAPI $
    runStreamHTTP ScopeHTTP {httpCallback}
      . api
      . Request

-- | Wai WebSocket Server App for GraphQL subscriptions
subscriptionApp ::
  ( MonadUnliftIO m,
    Eq channel
  ) =>
  ( Store (Event channel a) m ->
    (Scope WS (Event channel a) m -> m ()) ->
    m app
  ) ->
  (Input WS -> Stream WS (Event channel a) m) ->
  m (app, Event channel a -> m ())
subscriptionApp appWrapper api =
  do
    store <- initDefaultStore
    app <- appWrapper store (connectionThread api)
    pure
      ( app,
        publishEventWith store
      )

webSocketsWrapper ::
  (MonadUnliftIO m, MonadIO m) =>
  Store e m ->
  (Scope WS e m -> m ()) ->
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

-- | Wai WebSocket Server App for GraphQL subscriptions
webSocketsApp ::
  ( MonadIO m,
    MonadUnliftIO m,
    Eq channel
  ) =>
  (Input WS -> Stream WS (Event channel a) m) ->
  m (ServerApp, Event channel a -> m ())
webSocketsApp = subscriptionApp webSocketsWrapper
