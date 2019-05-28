module Data.Morpheus.Server
  ( socketGQL
  ) where

import           Control.Concurrent                  (MVar, newMVar)
import           Control.Exception                   (finally)
import           Control.Monad                       (forever)
import           Data.Morpheus                       (InputAction (..), OutputAction (..))
import           Data.Text                           (Text)
import           Network.WebSockets                  (Connection, ServerApp, acceptRequest, forkPingThread, receiveData,
                                                      sendTextData)

import           Data.Morpheus.Server.ClientRegister (ClientRegister, connectClient, disconnectClient, publishUpdates,
                                                      updateClientByID)
import           Data.Morpheus.Server.GQLClient      (ClientID, GQLClient (..))

type GQLMessage = Text

type GQLAPI = InputAction ClientID GQLMessage -> IO (OutputAction ClientID GQLMessage)

updateChannels :: ClientID -> [Text] -> MVar ClientRegister -> IO ()
updateChannels id' channel' = updateClientByID id' setChannel
  where
    setChannel client' = client' {clientChannels = channel'}

handleGQLResponse :: Connection -> MVar ClientRegister -> OutputAction Int Text -> IO ()
handleGQLResponse connection' state msg =
  case msg of
    Publish {actionChannelID = chanelId', actionPayload = message', mutationResponse = response'} ->
      sendTextData connection' response' >> publishUpdates chanelId' message' state
    Subscribe (clientId', channels') -> updateChannels clientId' channels' state
    NoEffect response' -> sendTextData connection' response'

queryHandler :: GQLAPI -> GQLClient -> MVar ClientRegister -> IO ()
queryHandler interpreter' GQLClient {clientConnection = connection', clientID = id'} state = forever handleRequest
  where
    handleRequest = do
      msg <- receiveData connection' >>= \x -> interpreter' (SocketConnection id' x)
      print msg
      handleGQLResponse connection' state msg

application :: GQLAPI -> MVar ClientRegister -> ServerApp
application interpreter' state pending = do
  connection' <- acceptRequest pending
  forkPingThread connection' 30
  client' <- connectClient connection' state
  finally (queryHandler interpreter' client' state) (disconnectClient client' state)

socketGQL :: GQLAPI -> IO ServerApp
socketGQL interpreter = application interpreter <$> newMVar []
