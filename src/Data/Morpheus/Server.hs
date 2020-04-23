{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE DataKinds             #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( webSocketsApp
  , httpAppWithEffect
  )
where

import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO
                                                , withRunInIO
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Network.WebSockets             ( Connection
                                                , PendingConnection
                                                , sendTextData
                                                , receiveData
                                                , ServerApp
                                                )
import qualified Network.WebSockets          as WS

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Resolving         
                                                ( GQLChannel(..) )
import           Data.Morpheus.Types.IO         ( MapAPI(..) )
import           Data.Morpheus.Types.Internal.Subscription
                                                ( connect
                                                , disconnect
                                                , Input(..)
                                                , Stream
                                                , Store(..)
                                                , Scope(..)
                                                , API(..)
                                                , runStreamHTTP
                                                , runStreamWS
                                                , acceptApolloRequest
                                                , initDefaultStore
                                                , publishEventWith
                                                )


-- support old version of Websockets
pingThread :: Connection -> IO () -> IO ()
#if MIN_VERSION_websockets(0,12,6)
pingThread connection = WS.withPingThread connection 30 (return ())
#else
pingThread connection = (WS.forkPingThread connection 30 >>)
#endif

defaultWSScope :: MonadIO m => Store e m -> Connection -> Scope 'Ws e m
defaultWSScope Store { writeStore } connection = WS 
  { listener = liftIO (receiveData connection)
  , callback = liftIO . sendTextData connection
  , update = writeStore
  }

httpAppWithEffect
  ::  
   ( MonadIO m,
     MapAPI a
   )
  => (e -> m ())
  -> (Input 'Http -> Stream 'Http e m)
  -> a
  -> m a
httpAppWithEffect httpCallback api 
  = mapAPI 
    ( runStreamHTTP HTTP { httpCallback }
    . api 
    . Request
    )

finallyM :: MonadUnliftIO m => m () -> m () -> m ()
finallyM loop end = withRunInIO $ \runIO -> finally (runIO loop) (runIO end)

-- | Wai WebSocket Server App for GraphQL subscriptions
webSocketsApp
  ::  ( MonadIO m 
      , MonadUnliftIO m
      , (Eq (StreamChannel e)) 
      , (GQLChannel e) 
      )
  => (Input 'Ws -> Stream 'Ws e m)
  -> m (ServerApp , e -> m ())
webSocketsApp api = withRunInIO handle
  where
    handle runIO  = do 
      store <- initDefaultStore      
      pure (wsApp store, publishEventWith store)
     where
      wsApp store pending = do
        connection <- acceptApolloRequest pending
        let scope = defaultWSScope store connection
        pingThread connection $ do
          input <- connect 
          runIO $ finallyM
            (inputLoop api scope input)
            (disconnect scope input)

inputLoop 
  :: Monad m 
  => (Input 'Ws -> Stream 'Ws e m) 
  -> Scope 'Ws e m 
  -> Input 'Ws 
  -> m ()
inputLoop api scope input
            = forever
            $ runStreamWS scope 
            $ api input