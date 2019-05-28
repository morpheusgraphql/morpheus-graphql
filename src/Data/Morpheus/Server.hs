module Data.Morpheus.Server
  ( socketApplication
  ) where

import           Control.Concurrent                  (MVar, newMVar)
import           Control.Exception                   (finally)
import           Control.Monad                       (forM_, forever)
import           Data.Morpheus                       (InputAction (..), OutputAction (..))
import           Data.Text                           (Text)
import           Network.WebSockets                  (ServerApp, acceptRequest, forkPingThread, receiveData,
                                                      sendTextData)

import           Data.Morpheus.Server.ClientRegister (ClientRegister, clientsByChannel, connectClient, disconnectClient,
                                                      updateClientByID)
import           Data.Morpheus.Server.GQLClient      (Channel, ClientID, GQLClient (..))

type GQLMessage = Text

type GQLAPI = InputAction ClientID GQLMessage -> IO (OutputAction ClientID GQLMessage)

publishUpdates :: Channel -> GQLMessage -> MVar ClientRegister -> IO ()
publishUpdates channelID' message state = do
  state' <- clientsByChannel channelID' state
  forM_ state' sendMessage
  where
    sendMessage (_, GQLClient {clientConnection = connection'}) = sendTextData connection' message

updateChannels :: ClientID -> [Text] -> MVar ClientRegister -> IO ()
updateChannels id' channel' = updateClientByID id' setChannel
  where
    setChannel client' = client' {clientChannels = channel'}

queryHandler :: GQLAPI -> GQLClient -> MVar ClientRegister -> IO ()
queryHandler interpreter' GQLClient {clientConnection = connection', clientID = id'} state = forever handleRequest
  where
    handleRequest = do
      msg <- receiveData connection' >>= \x -> interpreter' (SocketConnection id' x)
      print msg
      case msg of
        EffectPublish {actionChannelID = chanelId', actionPayload = message', actionMutationResponse = response'} ->
          sendTextData connection' response' >> publishUpdates chanelId' message' state
        NoEffectResult response' -> sendTextData connection' response'
        EffectSubscribe (clientId', channels') -> updateChannels clientId' channels' state

application :: GQLAPI -> MVar ClientRegister -> ServerApp
application interpreter' state pending = do
  connection' <- acceptRequest pending
  forkPingThread connection' 30
  client' <- connectClient connection' state
  finally (queryHandler interpreter' client' state) (disconnectClient client' state)

socketApplication :: GQLAPI -> IO ServerApp
socketApplication interpreter = application interpreter <$> newMVar []
