module Data.Morpheus.Server.ClientRegister
  ( ClientRegister
  , GQLState
  , initGQLState
  , connectClient
  , disconnectClient
  , updateClientByID
  , publishUpdates
  , updateClientSubscription
  ) where

import           Control.Concurrent                         (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Monad                              (forM_)
import           Data.List                                  (intersect)
import           Data.Morpheus.Server.GQLClient             (Channel, ClientID, GQLClient (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Text                                  (Text)
import           Data.UUID.V4                               (nextRandom)
import           Network.WebSockets                         (Connection, sendTextData)

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
      id' <- nextRandom
      return
        ( id'
        , GQLClient {clientID = id', clientConnection = connection', clientChannels = [], clientQuerySelection = []})
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

publishUpdates :: [Channel] -> (SelectionSet -> IO Text) -> GQLState -> IO ()
publishUpdates channels resolver' state = do
  state' <- clientsByChannel
  forM_ state' sendMessage
  where
    sendMessage (_, GQLClient {clientConnection = connection', clientQuerySelection = selection'}) =
      resolver' selection' >>= sendTextData connection'
    clientsByChannel :: IO ClientRegister
    clientsByChannel = filterByChannels <$> readMVar state
      where
        filterByChannels :: ClientRegister -> ClientRegister
        filterByChannels = filter (([] /=) . intersect channels . clientChannels . snd)

updateClientSubscription :: ClientID -> SelectionSet -> [Text] -> GQLState -> IO ()
updateClientSubscription id' selection' channel' = updateClientByID id' setChannel
  where
    setChannel client' = client' {clientChannels = channel', clientQuerySelection = selection'}
