{-# LANGUAGE NamedFieldPuns , FlexibleContexts #-}

module Data.Morpheus.Execution.Server.Subscription
  ( ClientDB
  , GQLState
  , initGQLState
  , connectClient
  , disconnectClient
  , updateClientByID
  , publishUpdates
  , addClientSubscription
  , removeClientSubscription
  )
where

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Concurrent             ( MVar
                                                , modifyMVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.List                      ( intersect )
import           Data.UUID.V4                   ( nextRandom )
import           Network.WebSockets             ( Connection
                                                , sendTextData
                                                )
import           Data.HashMap.Lazy              ( empty
                                                , insert
                                                , delete
                                                , adjust
                                                , toList
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Apollo
                                                ( toApolloResponse )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Event(..)
                                                , GQLChannel(..)
                                                , SubEvent
                                                )
import           Data.Morpheus.Types.Internal.WebSocket
                                                ( ClientID
                                                , GQLClient(..)
                                                , ClientDB
                                                , GQLState
                                                , SesionID
                                                )

-- | initializes empty GraphQL state
initGQLState :: IO (GQLState m e)
initGQLState = newMVar empty
 
connectClient :: MonadIO m => Connection -> GQLState m e -> IO (GQLClient m e)
connectClient clientConnection gqlState = do
  clientID <- nextRandom
  let client = GQLClient { clientID , clientConnection, clientSessions = empty }
  modifyMVar_ gqlState (pure . insert clientID client)
  return client

disconnectClient :: GQLClient m e -> GQLState m e -> IO (ClientDB m e)
disconnectClient GQLClient { clientID } state = modifyMVar state removeUser
 where
  removeUser db = let s' = delete clientID db in return (s', s')

updateClientByID
  :: MonadIO m =>
     ClientID
  -> (GQLClient m e -> GQLClient m e)
  -> MVar (ClientDB m e)
  -> m ()
updateClientByID key f state = liftIO $ modifyMVar_ state (return . adjust f key)


publishUpdates
  :: (Eq (StreamChannel e), GQLChannel e, MonadIO m) => GQLState m e -> e -> m ()
publishUpdates gqlState event = liftIO (readMVar gqlState) >>= traverse_ sendMessage 
 where
  sendMessage GQLClient { clientSessions, clientConnection } 
    | null clientSessions  = return ()
    | otherwise = mapM_ send (filterByChannels clientSessions)
   where
    send (sid, Event { content = subscriptionRes }) = do
      res <- subscriptionRes event
      let apolloRes = toApolloResponse sid res
      liftIO $ sendTextData clientConnection apolloRes
    ---------------------------
    filterByChannels = filter
      ( not
      . null
      . intersect (streamChannels event)
      . channels
      . snd
      ) . toList

removeClientSubscription :: MonadIO m => ClientID -> SesionID -> GQLState m e -> m ()
removeClientSubscription cid sid = updateClientByID cid stopSubscription
 where
  stopSubscription client = client { clientSessions = delete sid (clientSessions client) }

addClientSubscription
  :: MonadIO m => ClientID -> SubEvent m e -> SesionID -> GQLState m e -> m ()
addClientSubscription cid subscriptions sid = updateClientByID cid startSubscription
 where
  startSubscription client = client
    { clientSessions = insert sid subscriptions (clientSessions client) }
