{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , gqlSocketMonadIOApp
  , initGQLState
  , GQLState
  , statefulResolver
  )
where

import           Control.Monad                  ((>=>))
import           Data.ByteString.Lazy.Char8     (ByteString)
import           Data.Foldable                  ( traverse_ )
import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Network.WebSockets             ( ServerApp
                                                , Connection
                                                , sendTextData
                                                , receiveData
                                                )
import qualified Network.WebSockets          as WS
import           Control.Concurrent             ( MVar
                                                , readMVar
                                                , newMVar
                                                , modifyMVar_
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..)
                                                , ResponseEvent(..)
                                                , ResponseStream
                                                , ResultT(..)
                                                , unpackEvents
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                (empty)
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , coreResolver
                                                , EventCon
                                                , decodeNoDup
                                                )
import           Data.Morpheus.Types.Internal.Apollo
                                                ( acceptApolloRequest )
import           Data.Morpheus.Execution.Server.Subscription
                                                ( connect
                                                , disconnect
                                                , toResponseStream
                                                , traverseS
                                                , Action(..)
                                                , PubSubStore
                                                , OUT
                                                , IN
                                                , Stream(..)
                                                , publishEvents
                                                )
import           Data.Morpheus.Types.Internal.AST
import           Data.Morpheus.Types.IO
import           Data.Aeson                     ( encode )


runEffects 
  :: ( Executor store ref m
     , Monad m
     , EventCon e
     ) 
  => store ref e m -> [ResponseEvent e m] -> m ()
runEffects  store = traverse_ execute 
  where
    execute (Publish events) = runStream store (pure "") (const $ pure ()) (publishEvents events) 
    execute Subscribe{}      = pure ()

-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState e m = WSStore Connection e m -- SharedState

class Executor store ref m where
  run :: store ref e m -> Action OUT ref e m -> m ()

runStream 
  :: (Monad m, Executor store ref m) 
  => store ref e m 
  -> m ByteString
  -> (ByteString -> m ())
  -> Stream OUT ref e m 
  ->  m ()
runStream state receive callback Stream { stream }  
  = stream receive callback 
    >>= traverse_ (run state) 

-- | initializes empty GraphQL state
initGQLState :: (MonadIO m) => IO (WSStore ref e m)
initGQLState = WSStore <$> newMVar empty

newtype WSStore ref e m = WSStore { unWSStore :: MVar (PubSubStore e m) }

listen :: MonadIO m => Connection -> m ByteString
listen = liftIO . receiveData

notify :: MonadIO m => Connection -> ByteString -> m ()
notify conn = liftIO . sendTextData conn

readState :: (MonadIO m) => GQLState e m -> m (PubSubStore e m)
readState = liftIO . readMVar . unWSStore

modifyState_ 
  :: (MonadIO m) 
  => WSStore Connection e m 
  -> (PubSubStore e m -> PubSubStore  e m) 
  -> m ()
modifyState_ state changes = liftIO $ modifyMVar_ (unWSStore state) (return . changes)

instance MonadIO m => Executor WSStore Connection m where
  run state (Update changes)  
    = modifyState_ state changes
  run state (Notify runNotify)  
    = readState state 
      >>= runNotify
  run _ (Error x) = liftIO (print x)

-- support old version of Websockets
pingThread :: Connection -> IO () -> IO ()
#if MIN_VERSION_websockets(0,12,6)
pingThread connection = WS.withPingThread connection 30 (return ())
#else
pingThread connection = (WS.forkPingThread connection 30 >>)
#endif


streamApp
  ::  (RootResCon m e que mut sub
      , MonadIO m
      )
  => GQLRootResolver m e que mut sub
  -> Stream IN ref e m
  -> Stream OUT ref e m
streamApp root = traverseS (toResponseStream (coreResolver root))

statefulResolver
  ::  ( EventCon event
      , MonadIO m
      , Executor store ref m
      )
  => store ref event m
  -> (GQLRequest -> ResponseStream event m (Value VALID))
  -> ByteString
  -> m ByteString
statefulResolver store streamApi requestText = do
  res <- runResultT (decodeNoDup requestText >>= streamApi)
  runEffects store (unpackEvents res)
  pure $ encode $ renderResponse res

 
-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub
  -> GQLState e m
  -> (m () -> IO ())
  -> ServerApp
gqlSocketMonadIOApp root state f pending = do
  connection <- acceptApolloRequest pending
  pingThread connection $ do
      stream <- connect notify connection
      finally
        (handler connection stream) 
        $ f $ runStream state (listen connection) (notify connection) $ disconnect stream
 where
  handler conn inputStream
        = f
        $ forever
        $ runStream state 
            (listen conn)
            (notify conn)
        $ streamApp root inputStream 

-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState e IO
  -> ServerApp
gqlSocketApp gqlRoot state = gqlSocketMonadIOApp gqlRoot state id