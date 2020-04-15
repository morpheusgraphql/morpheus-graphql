{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns      #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , gqlSocketMonadIOApp
  , initGQLState
  , GQLState
  , statefulResolver
  )
where

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
                                                , Resolver
                                                , GQLChannel(..)
                                                , ResponseEvent(..)
                                                , ResponseStream
                                                , Eventless
                                                , cleanEvents
                                                , ResultT(..)
                                                , unpackEvents
                                                , Failure(..)
                                                , resolveUpdates
                                                , Context(..)
                                                , runResolverModel
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
                                                , Notification(..)
                                                , Action(..)
                                                , PubSubStore
                                                , IN
                                                , OUT
                                                , Stream(..)
                                                , publishEvents
                                                )
import           Data.Morpheus.Types.Internal.AST
import           Data.Morpheus.Types.IO
import           Data.Aeson                     ( encode )


-- TODO:
-- Stream IN m ByteString  -> Stream OUT m ByteString
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

runEffects 
  :: ( Executor store ref m
     , Monad m
     , EventCon e
     ) 
  => store ref e m -> [ResponseEvent e m] -> m ()
runEffects  store = traverse_ execute 
  where
    execute (Publish events) = runStream (publishEvents events) store
    execute Subscribe{}      = pure ()

-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState e m = WSStore Connection e m -- SharedState

class Executor store ref m where
  run :: store ref e m -> Action OUT ref e m -> m ()

runStream :: (Monad m, Executor store ref m) => Stream OUT ref e m -> store ref e m ->  m ()
runStream Stream { stream } state = traverse_ (run state) stream

-- | initializes empty GraphQL state
initGQLState :: (MonadIO m) => IO (WSStore ref e m)
initGQLState = WSStore <$> newMVar empty

newtype WSStore ref e m = WSStore { unWSStore :: MVar (PubSubStore ref e m) }

notify :: MonadIO m => Notification Connection m -> m ()
notify (Notification connection msg) = msg >>= liftIO . sendTextData connection

readState :: (MonadIO m) => GQLState e m -> m (PubSubStore Connection e m)
readState = liftIO . readMVar . unWSStore

modifyState_ 
  :: (MonadIO m) 
  => WSStore Connection e m 
  -> (PubSubStore Connection e m -> PubSubStore Connection e m) 
  -> m ()
modifyState_ state changes = liftIO $ modifyMVar_ (unWSStore state) (return . changes)

instance MonadIO m => Executor WSStore Connection m where
  run state (Update changes)  
    = modifyState_ state changes
  run state (Notify toNotification)  
    = readState state 
      >>= traverse_ notify  
        . toNotification
  run _ (Error x) = liftIO (print x)
  run state (Request con f) = liftIO (receiveData con) >>= f >>= (`runStream` state)

-- support old version of Websockets
pingThread :: Connection -> IO () -> IO ()
#if MIN_VERSION_websockets(0,12,6)
pingThread connection = WS.withPingThread connection 30 (return ())
#else
pingThread connection = (WS.forkPingThread connection 30 >>)
#endif

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
      stream <- connect connection
      finally
        (handler stream) 
        $ f (runStream (disconnect stream) state) 
 where
  handler st
        = f
        $ forever
        $ traverseS (toResponseStream  (coreResolver root)) st
        >>= (`runStream` state)

-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState e IO
  -> ServerApp
gqlSocketApp gqlRoot state = gqlSocketMonadIOApp gqlRoot state id
