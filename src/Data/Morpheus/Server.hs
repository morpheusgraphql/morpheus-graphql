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
  , httpPubApp
  )
where


import           Control.Monad.IO.Unlift        ( MonadUnliftIO
                                                , withRunInIO
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Network.WebSockets             ( Connection
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
                                                ( connectionThread
                                                , Input(..)
                                                , Stream
                                                , Store(..)
                                                , Scope(..)
                                                , HTTP
                                                , WS
                                                , runStreamHTTP
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

defaultWSScope :: MonadIO m => Store e m -> Connection -> Scope WS e m
defaultWSScope Store { writeStore } connection = ScopeWS 
  { listener = liftIO (receiveData connection)
  , callback = liftIO . sendTextData connection
  , update = writeStore
  }

httpPubApp
  ::  
   ( MonadIO m,
     MapAPI a
   )
  => (Input HTTP -> Stream HTTP e m)
  -> (e -> m ())
  -> a
  -> m a
httpPubApp api httpCallback  
  = mapAPI 
    ( runStreamHTTP ScopeHTTP { httpCallback }
    . api 
    . Request
    )

-- | Wai WebSocket Server App for GraphQL subscriptions
subscriptionApp
  ::  ( MonadIO m 
      , MonadUnliftIO m
      , (Eq (StreamChannel e)) 
      , (GQLChannel e) 
      )
  =>  ( Store e m  
        -> (Scope WS e m -> IO () ) 
        -> serverApp 
      )
  ->  ( Input WS -> Stream WS e m )
  ->  m ( serverApp, e -> m () )
subscriptionApp appWrapper api  
  = withRunInIO 
    $ \runIO -> do 
        store <- initDefaultStore      
        pure 
          ( appWrapper store (runIO . connectionThread api)
          , publishEventWith store
          )

webSocketsWrapper 
  :: (MonadUnliftIO m, MonadIO m)
  => Store e m 
  -> (Scope WS e m -> IO ())
  -> ServerApp
webSocketsWrapper store handler pending 
  = do
    connection <- acceptApolloRequest pending
    let scope = defaultWSScope store connection
    pingThread 
      connection 
      $ handler scope

-- | Wai WebSocket Server App for GraphQL subscriptions
webSocketsApp
  ::  ( MonadIO m 
      , MonadUnliftIO m
      , (Eq (StreamChannel e)) 
      , (GQLChannel e) 
      )
  => (Input WS -> Stream WS e m)
  -> m (ServerApp , e -> m ())
webSocketsApp = subscriptionApp webSocketsWrapper