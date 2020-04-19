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
                                                , Result(..)
                                                , unpackEvents
                                                , Eventless
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
                                                , toOutStream
                                                , Action(..)
                                                , PubSubStore
                                                , OUT
                                                , IN
                                                , Stream(..)
                                                , Scope(..)
                                                )
import           Data.Morpheus.Types.Internal.AST
import           Data.Morpheus.Types.IO
import           Data.Aeson                     ( encode )


-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState e m = WSStore Connection e m -- SharedState

class Executor store ref m where
  run :: store ref e m -> Action OUT ref e m -> m ()

runStream 
  :: (Monad m, Executor store ref m) 
  => store ref e m
  -> Scope m
  -> Stream OUT ref e m 
  ->  m (Eventless (Value VALID))
runStream state scope Stream { stream }  
  = do
    x <- runResultT (stream (pure ()) scope)
    case x of 
      Success  r w events-> do 
        traverse_ (run state) events
        pure (Success r w []) 
      Failure x -> pure $ Failure x 

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
  -> Action IN ref e m
  -> Stream OUT ref e m
streamApp root = toOutStream (coreResolver root)

statefulResolver
  ::  ( MonadIO m
      , Executor store ref m
      , RootResCon m e que mut sub
      )
  => store ref e m
  -> GQLRootResolver m e que mut sub
  -> GQLRequest
  -> m GQLResponse
statefulResolver store root request = 
  renderResponse 
    <$> runStream 
          store 
          HTTP 
          (streamApp root (Request request))

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (RootResCon m e que mut sub, MonadIO m)
  => (m () -> IO ())
  -> GQLRootResolver m e que mut sub
  -> GQLState e m
  -> ServerApp
gqlSocketMonadIOApp f root state pending = do
  connection <- acceptApolloRequest pending
  let scope = WS 
              { listener = listen connection
              , callback = notify connection
              }
  pingThread connection $ do
      action <- connect 
      finally
        (handler scope action) 
        $ f $ traverse_ (run state) (disconnect action)
 where
  handler scope inputAction
        = f
        $ forever
        $ runStream state scope 
        $ streamApp root inputAction

-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState e IO
  -> ServerApp
gqlSocketApp = gqlSocketMonadIOApp id