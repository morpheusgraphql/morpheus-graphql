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
  ( gqlSocketApp
  , gqlSocketMonadIOApp
  , initGQLState
  , Store(..)
  , statefull
  , storePublisher
  )
where

import           Data.Either
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
                                                ( ResultT(..)
                                                , Result(..)
                                                , GQLChannel(..)
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                (empty)
import           Data.Morpheus.Types.Internal.Apollo
                                                ( acceptApolloRequest )
import           Data.Morpheus.Types.Internal.Subscription
                                                (publish)
import           Data.Morpheus.Execution.Server.Subscription
                                                ( connect
                                                , disconnect
                                                , Action(..)
                                                , PubSubStore
                                                , Input(..)
                                                , Stream(..)
                                                , Scope(..)
                                                , API(..)
                                                )
import           Data.Morpheus.Types.IO         ( MapAPI(..)
                                                , GQLResponse
                                                , renderResponse
                                                )

-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
data Store e m = Store 
  { readStore :: m (PubSubStore e m)
  , writeStore :: (PubSubStore e m -> PubSubStore e m) -> m ()
  }

run :: Scope 'Ws e m -> Action e m -> m ()
run WS { update } (Update changes) = update changes

runStreamWS 
  :: (Monad m) 
  => Scope 'Ws e m
  -> Stream 'Ws e m 
  ->  m ()
runStreamWS scope@WS{ callback } StreamWS { streamWS }  
  = streamWS scope 
    >>= either callback (traverse_ (run scope))

runStreamHTTP
  :: (Monad m) 
  => Scope 'Http e m
  -> Stream 'Http e m 
  ->  m GQLResponse
runStreamHTTP scope StreamHTTP { streamHTTP }  
  = streamHTTP scope

-- | initializes empty GraphQL state
initGQLState :: 
  ( MonadIO m
  , (Eq (StreamChannel event)) 
  , (GQLChannel event) 
  ) 
  => IO (Store event m)
initGQLState = do
  store <- WSStore <$> newMVar empty
  pure Store 
    { readStore = readState store
    , writeStore = modifyState_ store
    }

storePublisher :: 
  ( MonadIO m
  , (Eq (StreamChannel event)) 
  , (GQLChannel event) 
  ) => Store event m -> event -> m ()
storePublisher store event = readStore store >>= publish event

newtype WSStore e m = WSStore { unWSStore :: MVar (PubSubStore e m) }

listen :: MonadIO m => Connection -> m ByteString
listen = liftIO . receiveData

notify :: MonadIO m => Connection -> ByteString -> m ()
notify conn = liftIO . sendTextData conn

readState :: (MonadIO m) => WSStore e m -> m (PubSubStore e m)
readState = liftIO . readMVar . unWSStore

modifyState_ 
  :: (MonadIO m) 
  => WSStore e m 
  -> (PubSubStore e m -> PubSubStore e m) 
  -> m ()
modifyState_ state changes = liftIO $ modifyMVar_ (unWSStore state) (return . changes)


-- support old version of Websockets
pingThread :: Connection -> IO () -> IO ()
#if MIN_VERSION_websockets(0,12,6)
pingThread connection = WS.withPingThread connection 30 (return ())
#else
pingThread connection = (WS.forkPingThread connection 30 >>)
#endif

statefull
  ::  
   ( MonadIO m,
     MapAPI a
   )
  => (e -> m ())
  -> (Input 'Http -> Stream 'Http e m)
  -> a
  -> m a
statefull httpCallback api 
  = mapAPI 
    ( runStreamHTTP HTTP { httpCallback }
    . api 
    . Request
    )

defaultWSScope :: MonadIO m => Store e m -> Connection -> Scope 'Ws e m
defaultWSScope Store { writeStore } connection = WS 
  { listener = listen connection
  , callback = notify connection
  , update = writeStore
  }

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (MonadIO m)
  => (m () -> IO ())
  -> (Input 'Ws -> Stream 'Ws e m)
  -> Store e m
  -> ServerApp
gqlSocketMonadIOApp f streamApp store pending = do
  connection <- acceptApolloRequest pending
  let scope = defaultWSScope store connection
  pingThread connection $ do
      input <- connect 
      finally
        (handler scope input) 
        $ f $ disconnect scope input
 where
  handler scope input
        = f
        $ forever
        $ runStreamWS scope 
        $ streamApp input

-- | Same as above but specific to IO
gqlSocketApp
  :: (Input 'Ws -> Stream 'Ws e IO)
  -> Store e IO
  -> ServerApp
gqlSocketApp = gqlSocketMonadIOApp id