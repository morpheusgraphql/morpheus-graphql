module Data.Morpheus.Server
  ( socketGQL
  , initGQLState
  ) where

import           Control.Exception                          (finally)
import           Control.Monad                              (forever)
import           Data.Morpheus.Server.ClientRegister        (GQLState, connectClient, disconnectClient, initGQLState,
                                                             publishUpdates, updateClientSubscription)
import           Data.Morpheus.Server.GQLClient             (GQLClient (..))
import           Data.Morpheus.StreamInterpreter            (InputAction (..), OutputAction (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Text                                  (Text)
import           Network.WebSockets                         (Connection, ServerApp, acceptRequest, forkPingThread,
                                                             receiveData, sendTextData)

type GQLAPI = InputAction Text -> IO (OutputAction Text)

handleGQLResponse :: Connection -> GQLState -> OutputAction Text -> IO ()
handleGQLResponse connection' state msg =
  case msg of
    PublishMutation {mutationChannels = channels', subscriptionResolver = resolver', mutationResponse = response'} ->
      sendTextData connection' response' >> publishUpdates channels' resolver' state
    InitSubscription { subscriptionClientID = clientId'
                     , subscriptionQuery = selection'
                     , subscriptionChannels = channels'
                     } -> updateClientSubscription clientId' selection' channels' state
    NoEffect response' -> sendTextData connection' response'

queryHandler :: GQLAPI -> GQLClient -> GQLState -> IO ()
queryHandler interpreter' GQLClient {clientConnection = connection', clientID = id'} state = forever handleRequest
  where
    handleRequest = do
      msg <- receiveData connection' >>= \x -> interpreter' (SocketInput id' x)
      -- print msg
      handleGQLResponse connection' state msg

socketGQL :: GQLAPI -> GQLState -> ServerApp
socketGQL interpreter' state pending = do
  connection' <- acceptRequest pending
  forkPingThread connection' 30
  client' <- connectClient connection' state
  finally (queryHandler interpreter' client' state) (disconnectClient client' state)
