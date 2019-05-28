module Data.Morpheus.Server.ClientRegister
  ( ClientRegister
  , connectClient
  , disconnectClient
  , updateClientByID
  , clientsByChannel
  ) where

import           Control.Concurrent             (MVar, modifyMVar, modifyMVar_, readMVar)
import           Data.Morpheus.Server.GQLClient (Channel, ClientID, GQLClient (..))
import           Network.WebSockets             (Connection)

type ClientRegister = [(ClientID, GQLClient)]

removeClient :: GQLClient -> ClientRegister -> ClientRegister
removeClient client = filter ((/= clientID client) . fst)

filterByChannel :: Channel -> ClientRegister -> ClientRegister
filterByChannel channelID' = filter (elem channelID' . clientChannels . snd)

disconnectClient :: GQLClient -> MVar ClientRegister -> IO ClientRegister
disconnectClient client state = modifyMVar state removeUser
  where
    removeUser state' =
      let s' = removeClient client state'
       in return (s', s')

connectClient :: Connection -> MVar ClientRegister -> IO GQLClient
connectClient connection' varState' = do
  client' <- newClient
  modifyMVar_ varState' (addClient client')
  return (snd client')
  where
    newClient = do
      id' <- length <$> readMVar varState' -- TODO: better uid
      return (id', GQLClient {clientID = id', clientConnection = connection', clientChannels = []})
    addClient client' state' = return (client' : state')

clientsByChannel :: Channel -> MVar ClientRegister -> IO ClientRegister
clientsByChannel channelID' state = filterByChannel channelID' <$> readMVar state

updateClientByID :: ClientID -> (GQLClient -> GQLClient) -> MVar ClientRegister -> IO ()
updateClientByID id' updateFunc state = modifyMVar_ state (return . map updateClient)
  where
    updateClient (key', client')
      | key' == id' = (key', updateFunc client')
    updateClient state' = state'
