{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server
  ( gqlSocketApp
  , initGQLState
  , GQLState
  ) where

import           Control.Exception                      (finally)
import           Control.Monad                          (forever)
import           Data.Morpheus.Server.Apollo            (ApolloSubscription (..), apolloProtocol, parseApolloRequest)
import           Data.Morpheus.Server.ClientRegister    (GQLState, connectClient, disconnectClient, initGQLState,
                                                         publishUpdates, updateClientSubscription)
import           Data.Morpheus.Types.Internal.WebSocket (GQLClient (..), InputAction (..), OutputAction (..))
import           Data.Text                              (Text)
import           Network.WebSockets                     (Connection, ServerApp, acceptRequestWith, forkPingThread,
                                                         receiveData, sendTextData)

type GQLAPI = InputAction Text -> IO (OutputAction Text)

handleGQLResponse :: Connection -> GQLState -> Int -> OutputAction Text -> IO ()
handleGQLResponse connection' state sessionId' msg =
  case msg of
    PublishMutation {mutationChannels = channels', subscriptionResolver = resolver', mutationResponse = response'} ->
      sendTextData connection' response' >> publishUpdates channels' resolver' state
    InitSubscription { subscriptionClientID = clientId'
                     , subscriptionQuery = selection'
                     , subscriptionChannels = channels'
                     } -> updateClientSubscription clientId' selection' channels' sessionId' state
    NoEffect response' -> sendTextData connection' response'

queryHandler :: GQLAPI -> GQLClient -> GQLState -> IO ()
queryHandler interpreter' GQLClient {clientConnection = connection', clientID = id'} state = forever handleRequest
  where
    handleRequest = do
      msg <- receiveData connection'
      case parseApolloRequest msg of
        Left x -> print x
        Right ApolloSubscription {apolloQuery = Nothing} -> return ()
        Right ApolloSubscription {apolloType = "subscription_end", apolloId = Just sid'} -> return () -- TODO: clear session ID
        Right ApolloSubscription {apolloType = "subscription_start", apolloId = Just sid'} ->
          interpreter' (SocketInput id' msg) >>= handleGQLResponse connection' state sid'
        Right _ -> return ()

gqlSocketApp :: GQLAPI -> GQLState -> ServerApp
gqlSocketApp interpreter' state pending = do
  connection' <- acceptRequestWith pending apolloProtocol
  forkPingThread connection' 30
  client' <- connectClient connection' state
  finally (queryHandler interpreter' client' state) (disconnectClient client' state)
