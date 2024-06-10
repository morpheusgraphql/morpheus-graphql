{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Subscriptions.Internal
  ( connect,
    disconnect,
    connectionThread,
    runStreamWS,
    runStreamHTTP,
    ApiContext (..),
    Input (..),
    PUB,
    SUB,
    Store (..),
    ClientConnectionStore,
    acceptApolloRequest,
    SessionID,
    publish,
    initDefaultStore,
    publishEventWith,
    empty,
    toList,
    connectionSessionIds,
    storedSessions,
    storedChannels,
    streamApp,
    ApolloSubscription (..),
    ApolloAction (..),
  )
where

import Control.Concurrent
  ( modifyMVar_,
  )
import Control.Exception (finally)
import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
    withRunInIO,
  )
-- MORPHEUS

import Data.Morpheus.App
  ( App,
    runAppStream,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Subscriptions.Apollo
  ( ApolloAction (..),
    ApolloSubscription (..),
    acceptApolloRequest,
  )
import Data.Morpheus.Subscriptions.ClientConnectionStore
  ( ClientConnectionStore,
    SessionID,
    connectionSessionIds,
    delete,
    publish,
    storedChannels,
    storedSessions,
    toList,
  )
import Data.Morpheus.Subscriptions.Event
  ( Event,
  )
import Data.Morpheus.Subscriptions.Stream
  ( ApiContext (..),
    Input (..),
    Output,
    PUB,
    SUB,
    runStreamHTTP,
    runStreamWS,
    toOutStream,
  )
import Data.UUID.V4 (nextRandom)
import Relude hiding (empty, toList)

connect :: (MonadIO m) => m (Input SUB)
connect = InitConnection <$> liftIO nextRandom

disconnect :: ApiContext SUB e m -> Input SUB -> m ()
disconnect SubContext {updateStore} (InitConnection clientID) =
  updateStore (delete clientID)

-- | PubSubStore interface
-- shared GraphQL state between __websocket__ and __http__ server,
-- you can define your own store if you provide write and read methods
-- to work properly Morpheus needs all entries of ClientConnectionStore (+ client Callbacks)
-- that why it is recommended that you use many local ClientStores on every server node
-- rather then single centralized Store.
data Store e m = Store
  { readStore :: m (ClientConnectionStore e m),
    writeStore :: (ClientConnectionStore e m -> ClientConnectionStore e m) -> m ()
  }

publishEventWith ::
  ( MonadIO m,
    Eq channel,
    Hashable channel,
    Show channel
  ) =>
  Store (Event channel cont) m ->
  Event channel cont ->
  m ()
publishEventWith store event = readStore store >>= publish event

-- | initializes empty GraphQL state
initDefaultStore ::
  ( MonadIO m,
    MonadIO m2
  ) =>
  m2 (Store (Event ch con) m)
initDefaultStore = do
  store <- liftIO $ newMVar empty
  pure
    Store
      { readStore = liftIO $ readMVar store,
        writeStore = \changes -> liftIO $ modifyMVar_ store (return . changes)
      }

finallyM :: (MonadUnliftIO m) => m () -> m () -> m ()
finallyM loop end = withRunInIO $ \runIO -> finally (runIO loop) (runIO end)

connectionThread ::
  ( MonadUnliftIO m,
    Eq ch,
    Hashable ch
  ) =>
  App (Event ch con) m ->
  ApiContext SUB (Event ch con) m ->
  m ()
connectionThread api scope = do
  input <- connect
  finallyM
    (connectionLoop api scope input)
    (disconnect scope input)

connectionLoop ::
  (Monad m, Eq ch, Hashable ch) =>
  App (Event ch con) m ->
  ApiContext SUB (Event ch con) m ->
  Input SUB ->
  m ()
connectionLoop app scope =
  forever
    . runStreamWS scope
    . streamApp app

streamApp ::
  (Eq ch, Monad m, Hashable ch) =>
  App (Event ch con) m ->
  Input api ->
  Output api (Event ch con) m
streamApp app = toOutStream (runAppStream app)
