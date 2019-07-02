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
import           Control.Monad                          (forM_)
import           Data.List                              (intersect)
import           Data.Morpheus.Server.Apollo            (toApolloResponse)
import           Data.Morpheus.Types.Internal.WebSocket (ClientID, ClientSession (..), GQLClient (..),
                                                         WSSubscription (..))
import           Data.UUID.V4                           (nextRandom)
import           Network.WebSockets                     (Connection, sendTextData)

type ClientRegister m s = [(ClientID, GQLClient m s)]

-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState m s = MVar (ClientRegister m s) -- SharedState

-- | initializes empty GraphQL state
initGQLState :: IO (GQLState m s)
initGQLState = newMVar []

connectClient :: Connection -> GQLState m s -> IO (GQLClient m s)
connectClient clientConnection varState' = do
  client' <- newClient
  modifyMVar_ varState' (addClient client')
  return (snd client')
  where
    newClient = do
      clientID <- nextRandom
      return (clientID, GQLClient {clientID, clientConnection, clientSessions = []})
    addClient client' state' = return (client' : state')

disconnectClient :: GQLClient m s -> GQLState m s -> IO (ClientRegister m s)
disconnectClient client state = modifyMVar state removeUser
  where
    removeUser state' =
      let s' = removeClient state'
       in return (s', s')
    removeClient :: ClientRegister m s -> ClientRegister m s
    removeClient = filter ((/= clientID client) . fst)

updateClientByID :: ClientID -> (GQLClient m s -> GQLClient m s) -> MVar (ClientRegister m s) -> IO ()
updateClientByID id' updateFunc state = modifyMVar_ state (return . map updateClient)
  where
    updateClient (key', client')
      | key' == id' = (key', updateFunc client')
    updateClient state' = state'

publishUpdates :: (Eq s) => [s] -> GQLState IO s -> IO ()
publishUpdates channels state = do
  state' <- readMVar state
  forM_ state' sendMessage
  where
    sendMessage (_, GQLClient {clientSessions = []}) = return ()
    sendMessage (_, GQLClient {clientSessions, clientConnection}) = mapM_ __send (filterByChannels clientSessions)
      where
        __send ClientSession {sessionId, sessionSubscription = WSSubscription {subscriptionRes}} =
          subscriptionRes (head channels) >>= sendTextData clientConnection . toApolloResponse sessionId
        filterByChannels = filter (([] /=) . intersect channels . subscriptionChannels . sessionSubscription)

removeClientSubscription :: ClientID -> Int -> GQLState m s -> IO ()
removeClientSubscription id' sid' = updateClientByID id' stopSubscription
  where
    stopSubscription client' = client' {clientSessions = filter ((sid' /=) . sessionId) (clientSessions client')}

addClientSubscription :: ClientID -> WSSubscription m s -> Int -> GQLState m s -> IO ()
addClientSubscription id' sessionSubscription sessionId = updateClientByID id' startSubscription
  where
    startSubscription client' =
      client' {clientSessions = ClientSession {sessionId, sessionSubscription} : clientSessions client'}
