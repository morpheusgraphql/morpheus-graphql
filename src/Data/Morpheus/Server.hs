module Data.Morpheus.Server
  ( socketApplication
  ) where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Morpheus      (InputAction (..), OutputAction (..))
import           Data.Text          (Text)
import           Network.WebSockets (Connection, ServerApp, acceptRequest, forkPingThread, receiveData, sendTextData)

type ClientID = Int

type ChannelID = Text

type GQLMessage = Text

data SocketClient = SocketClient
  { clientID         :: ClientID
  , clientConnection :: Connection
  , clientChannels   :: [Text]
  }

type ServerState = [(ClientID, SocketClient)]

generateID :: [a] -> Int
generateID = length

removeClient :: SocketClient -> ServerState -> ServerState
removeClient client = filter ((/= clientID client) . fst)

disconnectClient :: SocketClient -> MVar ServerState -> IO ServerState
disconnectClient client state = modifyMVar state removeUser
  where
    removeUser state' =
      let s' = removeClient client state'
       in return (s', s')

filterByChannel :: ChannelID -> ServerState -> ServerState
filterByChannel channelID' = filter (elem channelID' . clientChannels . snd)

publishUpdates :: ChannelID -> GQLMessage -> ServerState -> IO ()
publishUpdates channelID' message clients = forM_ (filterByChannel channelID' clients) sendMessage
  where
    sendMessage (_, SocketClient {clientConnection = connection'}) = sendTextData connection' message

type GQLApi = InputAction ClientID Text -> IO (OutputAction ClientID Text)

talk :: GQLApi -> SocketClient -> MVar ServerState -> IO ()
talk interpreter' SocketClient {clientConnection = connection', clientID = id'} state = forever handleRequest
  where
    handleRequest = do
      msg <- receiveData connection' >>= \x -> interpreter' (SocketConnection id' x)
      print msg
      case msg of
        EffectPublish chanelId' value'   -> readMVar state >>= publishUpdates chanelId' value' >> return ()
        NoEffectResult value'            -> sendTextData connection' value'
        EffectSubscribe (cid', channel') -> updateChannels cid' channel' <$> readMVar state >> return ()

updateChannels :: ClientID -> [Text] -> ServerState -> ServerState
updateChannels id' channel' = map setChannel
  where
    setChannel (key', client')
      | key' == id' = (key', client' {clientChannels = channel'})
    setChannel state' = state'

registerSubscription :: MVar ServerState -> Connection -> IO SocketClient
registerSubscription varState' connection' = do
  client' <- newClient
  modifyMVar_ varState' (addClient client')
  return (snd client')
  where
    newClient = do
      id' <- generateID <$> readMVar varState'
      return (id', SocketClient {clientID = id', clientConnection = connection', clientChannels = []})
    addClient client' state' = return (client' : state')

application :: GQLApi -> MVar ServerState -> ServerApp
application interpreter' state pending = do
  connection' <- acceptRequest pending
  forkPingThread connection' 30
  client' <- registerSubscription state connection'
  finally (talk interpreter' client' state) (disconnectClient client' state)

socketApplication :: GQLApi -> IO ServerApp
socketApplication interpreter = application interpreter <$> newMVar []
