module Data.Morpheus.Server.ClientRegister
  ( ClientRegister
  , GQLState
  , initGQLState
  , connectClient
  , disconnectClient
  , updateClientByID
  , publishUpdates
  , updateClientChannels
  ) where

import           Control.Concurrent             (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Monad                  (forM_)
import           Data.Morpheus.Server.GQLClient (Channel, ClientID, GQLClient (..))
import           Data.Text                      (Text)
import           Network.WebSockets             (Connection, sendTextData)

type ClientRegister = [(ClientID, GQLClient)]

type GQLState = MVar ClientRegister -- SharedState

initGQLState :: IO GQLState
initGQLState = newMVar []

connectClient :: Connection -> GQLState -> IO GQLClient
connectClient connection' varState' = do
  client' <- newClient
  modifyMVar_ varState' (addClient client')
  return (snd client')
  where
    newClient = do
      id' <- length <$> readMVar varState' -- TODO: better uid
      return (id', GQLClient {clientID = id', clientConnection = connection', clientChannels = []})
    addClient client' state' = return (client' : state')

disconnectClient :: GQLClient -> GQLState -> IO ClientRegister
disconnectClient client state = modifyMVar state removeUser
  where
    removeUser state' =
      let s' = removeClient state'
       in return (s', s')
    removeClient :: ClientRegister -> ClientRegister
    removeClient = filter ((/= clientID client) . fst)

updateClientByID :: ClientID -> (GQLClient -> GQLClient) -> MVar ClientRegister -> IO ()
updateClientByID id' updateFunc state = modifyMVar_ state (return . map updateClient)
  where
    updateClient (key', client')
      | key' == id' = (key', updateFunc client')
    updateClient state' = state'

publishUpdates :: Channel -> Text -> GQLState -> IO ()
publishUpdates channelID' message state = do
  state' <- clientsByChannel
  forM_ state' sendMessage
  where
    sendMessage (_, GQLClient {clientConnection = connection'}) = sendTextData connection' message
    clientsByChannel :: IO ClientRegister
    clientsByChannel = filterByChannel <$> readMVar state
      where
        filterByChannel :: ClientRegister -> ClientRegister
        filterByChannel = filter (elem channelID' . clientChannels . snd)

updateClientChannels :: ClientID -> [Text] -> GQLState -> IO ()
updateClientChannels id' channel' = updateClientByID id' setChannel
  where
    setChannel client' = client' {clientChannels = channel'}
