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
                                                )
import qualified Network.WebSockets          as WS

-- MORPHEUS
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

finallyUnIO :: MonadUnliftIO m => m () -> m () -> m ()
finallyUnIO loop end = withRunInIO $ \runIO -> finally (runIO loop) (runIO end)

pingThreadUnIO :: MonadUnliftIO m => Connection -> m () -> m ()
pingThreadUnIO conn x = withRunInIO $ \runIO -> pingThread conn (runIO x) 


-- | Wai WebSocket Server App for GraphQL subscriptions
webSocketsApp
  ::  ( MonadIO m 
      , MonadUnliftIO m
      )
  => (Input 'Ws -> Stream 'Ws e m)
  -> Store e m
  -- WebSocket App 
  -> PendingConnection 
  -> m ()
webSocketsApp streamApp store pending = do
  connection <- acceptApolloRequest pending
  let scope = defaultWSScope store connection
  pingThreadUnIO connection $ do
      input <- connect 
      finallyUnIO
        (handler scope input) 
        $ disconnect scope input
 where
  handler scope input
        = forever
        $ runStreamWS scope 
        $ streamApp input