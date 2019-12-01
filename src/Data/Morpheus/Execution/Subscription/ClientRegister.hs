{-# LANGUAGE NamedFieldPuns , FlexibleContexts #-}

module Data.Morpheus.Execution.Subscription.ClientRegister
  ( ClientRegister
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
import           Data.Text                      ( Text )
import           Data.UUID.V4                   ( nextRandom )
import           Network.WebSockets             ( Connection
                                                , sendTextData
                                                )
-- MORPHEUS
import           Data.Morpheus.Execution.Subscription.Apollo
                                                ( toApolloResponse )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Event(..)
                                                , GQLChannel(..)
                                                , SubEvent
                                                )
import           Data.Morpheus.Types.Internal.WebSocket
                                                ( ClientID
                                                , ClientSession(..)
                                                , GQLClient(..)
                                                )

type ClientRegister m e = [(ClientID, GQLClient m e)]

-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState m e = MVar (ClientRegister m e) -- SharedState

-- | initializes empty GraphQL state
initGQLState :: IO (GQLState m e)
initGQLState = newMVar []

connectClient :: MonadIO m => Connection -> GQLState m e -> IO (GQLClient m e)
connectClient clientConnection varState' = do
  client' <- newClient
  modifyMVar_ varState' (addClient client')
  return (snd client')
 where
  newClient = do
    clientID <- nextRandom
    return
      (clientID, GQLClient { clientID, clientConnection, clientSessions = [] })
  addClient client' state' = return (client' : state')

disconnectClient :: GQLClient m e -> GQLState m e -> IO (ClientRegister m e)
disconnectClient client state = modifyMVar state removeUser
 where
  removeUser state' = let s' = removeClient state' in return (s', s')
  removeClient :: ClientRegister m e -> ClientRegister m e
  removeClient = filter ((/= clientID client) . fst)

updateClientByID
  :: MonadIO m =>
     ClientID
  -> (GQLClient m e -> GQLClient m e)
  -> MVar (ClientRegister m e)
  -> m ()
updateClientByID id' updateFunc state = liftIO $ modifyMVar_
  state
  (return . map updateClient)
 where
  updateClient (key, client') | key == id' = (key, updateFunc client')
  updateClient state'                      = state'

publishUpdates
  :: (Eq (StreamChannel e), GQLChannel e, MonadIO m) => GQLState m e -> e -> m ()
publishUpdates state event = do
  state' <- liftIO $ readMVar state
  traverse_ sendMessage state'
 where
  sendMessage (_, GQLClient { clientSessions = [] }             ) = return ()
  sendMessage (_, GQLClient { clientSessions, clientConnection }) = mapM_
    __send
    (filterByChannels clientSessions)
   where
    __send ClientSession { sessionId, sessionSubscription = Event { content = subscriptionRes } } = do
      res <- subscriptionRes event
      let apolloRes = toApolloResponse sessionId res
      liftIO $ sendTextData clientConnection apolloRes
    ---------------------------
    filterByChannels = filter
      ( not
      . null
      . intersect (streamChannels event)
      . channels
      . sessionSubscription
      )

removeClientSubscription :: MonadIO m => ClientID -> Text -> GQLState m e -> m ()
removeClientSubscription id' sid' = updateClientByID id' stopSubscription
 where
  stopSubscription client' = client'
    { clientSessions = filter ((sid' /=) . sessionId) (clientSessions client')
    }

addClientSubscription
  :: MonadIO m => ClientID -> SubEvent m e -> Text -> GQLState m e -> m ()
addClientSubscription id' sessionSubscription sessionId = updateClientByID
  id'
  startSubscription
 where
  startSubscription client' = client'
    { clientSessions = ClientSession { sessionId, sessionSubscription }
                         : clientSessions client'
    }
