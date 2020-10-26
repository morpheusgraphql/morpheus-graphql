{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Subscription.Internal
  ( connect,
    disconnect,
    connectionThread,
    runStreamWS,
    runStreamHTTP,
    Scope (..),
    Input (..),
    WS,
    HTTP,
    acceptApolloRequest,
    publish,
    Store (..),
    initDefaultStore,
    publishEventWith,
    ClientConnectionStore,
    empty,
    toList,
    connectionSessionIds,
    SessionID,
    -- streamApp,
    SubscriptionApp (..),
    storedSessions,
    storedChannels,
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

import Data.Morpheus.Core
  ( App,
    runAppStream,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Subscription.Apollo
  ( acceptApolloRequest,
  )
import Data.Morpheus.Subscription.ClientConnectionStore
  ( ClientConnectionStore,
    SessionID,
    connectionSessionIds,
    delete,
    publish,
    storedChannels,
    storedSessions,
    toList,
  )
import Data.Morpheus.Subscription.Event
  ( Event,
  )
import Data.Morpheus.Subscription.Stream
  ( HTTP,
    Input (..),
    Scope (..),
    Stream,
    WS,
    runStreamHTTP,
    runStreamWS,
    toOutStream,
  )
import Data.UUID.V4 (nextRandom)
import Relude hiding (empty, toList)

connect :: MonadIO m => m (Input WS)
connect = Init <$> liftIO nextRandom

disconnect :: Scope WS e m -> Input WS -> m ()
disconnect ScopeWS {update} (Init clientID) = update (delete clientID)

-- | PubSubStore interface
-- shared GraphQL state between __websocket__ and __http__ server,
-- you can define your own store if you provide write and read methods
-- to work properly Morpheus needs all entries of ClientConnectionStore (+ client Callbacks)
-- that why it is recomended that you use many local ClientStores on evenry server node
-- rathen then single centralized Store.
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

finallyM :: MonadUnliftIO m => m () -> m () -> m ()
finallyM loop end = withRunInIO $ \runIO -> finally (runIO loop) (runIO end)

connectionThread ::
  ( MonadUnliftIO m,
    Eq ch,
    Hashable ch
  ) =>
  App (Event ch con) m ->
  Scope WS (Event ch con) m ->
  m ()
connectionThread api scope = do
  input <- connect
  finallyM
    (connectionLoop api scope input)
    (disconnect scope input)

connectionLoop ::
  (Monad m, Eq ch, Hashable ch) =>
  App (Event ch con) m ->
  Scope WS (Event ch con) m ->
  Input WS ->
  m ()
connectionLoop app scope input =
  forever
    $ runStreamWS scope
    $ streamApp app input

-- streamApp :: Monad m => App (Event ch con) m -> Input api -> Stream api (Event ch con) m
-- streamApp app = toOutStream (runAppStream app)

class SubscriptionApp (e :: *) where
  streamApp :: Monad m => App e m -> Input api -> Stream api e m

instance (Eq ch, Hashable ch) => SubscriptionApp (Event ch con) where
  streamApp app = toOutStream (runAppStream app)

instance SubscriptionApp () where
  streamApp _ = undefined
