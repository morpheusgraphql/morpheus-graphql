{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server
  ( socketApplication
  ) where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Morpheus      (InputAction (..), OutputAction (..))
import           Data.Text          (Text, pack)
import           Network.WebSockets (Connection, ServerApp, acceptRequest, forkPingThread, receiveData, sendTextData)

type ClientID = Text

data SocketClient = SocketClient
  { clientID         :: ClientID
  , clientConnection :: Connection
  , clientChannels   :: [Text]
  } deriving (Show)

type ServerState = [(Text, SocketClient)]

removeClient :: SocketClient -> ServerState -> ServerState
removeClient client = filter ((/= clientID client) . fst)

disconnectClient :: SocketClient -> MVar ServerState -> IO ServerState
disconnectClient client state = modifyMVar state removeUser
  where
    removeUser state' =
      let s' = removeClient client state'
       in return (s', s')

generateID :: [a] -> Text
generateID = ("connection_" <>) . pack . show . length

broadcast :: Text -> ServerState -> IO ServerState
broadcast message clients = do
  forM_ clients sendMessage
  return clients
  where
    sendMessage (_, client') = sendTextData (clientConnection client') message

instance Show Connection where
  show = const "Connection"

type GQLApi = InputAction ClientID Text -> IO (OutputAction ClientID Text)

talk :: GQLApi -> SocketClient -> MVar ServerState -> IO ()
talk interpreter' SocketClient {clientConnection = connection', clientID = id'} state = forever handleRequest
  where
    handleRequest = do
      msg <- receiveData connection' >>= \x -> interpreter' (SocketConnection id' x)
      print msg
      case msg of
        EffectPublish _ value'          -> readMVar state >>= broadcast value'
        NoEffectResult value'           -> sendTextData connection' value' >> readMVar state
        EffectSubscribe (channel', id') -> readMVar state

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
