{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Types.Internal.Subscription
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
    streamApp,
  )
where

import Control.Concurrent
  ( modifyMVar_,
    newMVar,
    readMVar,
  )
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
    withRunInIO,
  )
-- MORPHEUS

import Data.Morpheus.Core
  ( App,
    defaultConfig,
    runAppWith,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Event,
  )
import Data.Morpheus.Types.Internal.Subscription.Apollo
  ( acceptApolloRequest,
  )
import Data.Morpheus.Types.Internal.Subscription.ClientConnectionStore
  ( ClientConnectionStore,
    SessionID,
    connectionSessionIds,
    delete,
    publish,
    toList,
  )
import Data.Morpheus.Types.Internal.Subscription.Stream
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

streamApp :: Monad m => App e m -> Input api -> Stream api e m
streamApp app = toOutStream (runAppWith app defaultConfig)

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
  (MonadIO m, Eq channel) =>
  Store (Event channel cont) m ->
  Event channel cont ->
  m ()
publishEventWith store event = readStore store >>= publish event

-- | initializes empty GraphQL state
initDefaultStore ::
  ( MonadIO m,
    MonadIO m2
  ) =>
  m2 (Store event m)
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
  ( MonadUnliftIO m
  ) =>
  App e m ->
  Scope WS e m ->
  m ()
connectionThread api scope = do
  input <- connect
  finallyM
    (connectionLoop api scope input)
    (disconnect scope input)

connectionLoop ::
  Monad m =>
  App e m ->
  Scope WS e m ->
  Input WS ->
  m ()
connectionLoop app scope input =
  forever
    $ runStreamWS scope
    $ streamApp app input
