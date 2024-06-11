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
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Subscriptions.WebSockets
  ( webSocketsWrapper,
  )
where

import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
    withRunInIO,
  )
import Data.Morpheus.Subscriptions.Internal
  ( ApiContext (..),
    SUB,
    Store (..),
    acceptApolloRequest,
  )
import Network.WebSockets
  ( Connection,
    ServerApp,
    receiveData,
    sendTextData,
  )
import qualified Network.WebSockets as WS
import Relude

-- support old version of WebSockets
pingThread :: Connection -> IO () -> IO ()

#if MIN_VERSION_websockets(0,12,6)
pingThread connection = WS.withPingThread connection 30 (return ())
#else
pingThread connection = (WS.forkPingThread connection 30 >>)
#endif

defaultWSScope :: (MonadIO m) => Store e m -> Connection -> ApiContext SUB e m
defaultWSScope Store {writeStore} connection =
  SubContext
    { listener = liftIO (receiveData connection),
      callback = liftIO . sendTextData connection,
      updateStore = writeStore
    }

webSocketsWrapper ::
  (MonadUnliftIO m, MonadIO m) =>
  Store e m ->
  (ApiContext SUB e m -> m ()) ->
  m ServerApp
webSocketsWrapper store handler =
  withRunInIO
    $ \runIO ->
      pure
        $ \pending -> do
          conn <- acceptApolloRequest pending
          pingThread
            conn
            $ runIO (handler (defaultWSScope store conn))
