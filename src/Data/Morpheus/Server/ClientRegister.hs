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

import           Control.Concurrent                         (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Monad                              (forM_)
import           Data.List                                  (intersect)
import           Data.Morpheus.Server.Apollo                (toApolloResponse)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.WebSocket     (Channel, ClientID, ClientSession (..), GQLClient (..))
import           Data.Morpheus.Types.Response               (GQLResponse (..))
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
      return (id', GQLClient {clientID = id', clientConnection = connection', clientSessions = []})
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

publishUpdates :: [Channel] -> (SelectionSet -> IO GQLResponse) -> GQLState -> IO ()
publishUpdates channels resolver' state = do
  state' <- readMVar state
  forM_ state' sendMessage
  where
    sendMessage (_, GQLClient {clientSessions = []}) = return ()
    sendMessage (_, GQLClient {clientSessions = sessions', clientConnection = connection'}) =
      mapM_ __send (filterByChannels sessions')
      where
        __send ClientSession {sessionQuerySelection = selection', sessionId = sid'} =
          resolver' selection' >>= sendTextData connection' . toApolloResponse sid'
        filterByChannels :: [ClientSession] -> [ClientSession]
        filterByChannels = filter (([] /=) . intersect channels . sessionChannels)

removeClientSubscription :: ClientID -> Int -> GQLState -> IO ()
removeClientSubscription id' sid' = updateClientByID id' stopSubscription
  where
    stopSubscription client' = client' {clientSessions = filter ((sid' /=) . sessionId) (clientSessions client')}

addClientSubscription :: ClientID -> SelectionSet -> [Text] -> Int -> GQLState -> IO ()
addClientSubscription id' selection' channel' sid' = updateClientByID id' startSubscription
  where
    startSubscription client' =
      client'
        { clientSessions =
            ClientSession {sessionId = sid', sessionChannels = channel', sessionQuerySelection = selection'} :
            clientSessions client'
        }
