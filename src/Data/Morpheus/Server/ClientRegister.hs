{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Server.ClientRegister
  ( ClientRegister
  , GQLState
  , initGQLState
  , connectClient
  , disconnectClient
  , updateClientByID
  , publishUpdates
  , addClientSubscription
  , removeClientSubscription
  ) where

import           Control.Concurrent                     (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Data.Foldable                          (traverse_)
import           Data.List                              (intersect)
import           Data.Text                              (Text)
import           Data.UUID.V4                           (nextRandom)
import           Network.WebSockets                     (Connection, sendTextData)

-- MORPHEUS
import           Data.Morpheus.Server.Apollo            (toApolloResponse)
import           Data.Morpheus.Types.Internal.Stream    (Event (..), PubEvent, SubEvent)
import           Data.Morpheus.Types.Internal.WebSocket (ClientID, ClientSession (..), GQLClient (..))

type ClientRegister m e c = [(ClientID, GQLClient m e c)]

-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState m e c = MVar (ClientRegister m e c) -- SharedState

-- | initializes empty GraphQL state
initGQLState :: IO (GQLState m e c)
initGQLState = newMVar []

connectClient :: Connection -> GQLState m e c -> IO (GQLClient m e c)
connectClient clientConnection varState' = do
  client' <- newClient
  modifyMVar_ varState' (addClient client')
  return (snd client')
  where
    newClient = do
      clientID <- nextRandom
      return (clientID, GQLClient {clientID, clientConnection, clientSessions = []})
    addClient client' state' = return (client' : state')

disconnectClient :: GQLClient m e c -> GQLState m e c -> IO (ClientRegister m e c)
disconnectClient client state = modifyMVar state removeUser
  where
    removeUser state' =
      let s' = removeClient state'
       in return (s', s')
    removeClient :: ClientRegister m e c -> ClientRegister m e c
    removeClient = filter ((/= clientID client) . fst)

updateClientByID :: ClientID -> (GQLClient m e c -> GQLClient m e c) -> MVar (ClientRegister m e c) -> IO ()
updateClientByID id' updateFunc state = modifyMVar_ state (return . map updateClient)
  where
    updateClient (key', client')
      | key' == id' = (key', updateFunc client')
    updateClient state' = state'

publishUpdates :: (Eq e) => GQLState IO e c -> PubEvent e c -> IO ()
publishUpdates state event = do
  state' <- readMVar state
  traverse_ sendMessage state'
  where
    sendMessage (_, GQLClient {clientSessions = []}) = return ()
    sendMessage (_, GQLClient {clientSessions, clientConnection}) = mapM_ __send (filterByChannels clientSessions)
      where
        __send ClientSession {sessionId, sessionSubscription = Event {content = subscriptionRes}} =
          subscriptionRes event >>= sendTextData clientConnection . toApolloResponse sessionId
        filterByChannels = filter (([] /=) . intersect (channels event) . channels . sessionSubscription)

removeClientSubscription :: ClientID -> Text -> GQLState m e c -> IO ()
removeClientSubscription id' sid' = updateClientByID id' stopSubscription
  where
    stopSubscription client' = client' {clientSessions = filter ((sid' /=) . sessionId) (clientSessions client')}

addClientSubscription :: ClientID -> SubEvent m e c -> Text -> GQLState m e c -> IO ()
addClientSubscription id' sessionSubscription sessionId = updateClientByID id' startSubscription
  where
    startSubscription client' =
      client' {clientSessions = ClientSession {sessionId, sessionSubscription} : clientSessions client'}
