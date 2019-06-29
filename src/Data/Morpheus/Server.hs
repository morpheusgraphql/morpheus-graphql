{-# LANGUAGE OverloadedStrings #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , initGQLState
  , GQLState
  , GQLAPI
  ) where

import           Control.Exception                      (finally)
import           Control.Monad                          (forever)
import           Data.ByteString.Lazy.Char8             (ByteString)
import           Data.Morpheus.Server.Apollo            (ApolloSubscription (..), apolloProtocol, parseApolloRequest)
import           Data.Morpheus.Server.ClientRegister    (GQLState, addClientSubscription, connectClient,
                                                         disconnectClient, initGQLState, publishUpdates,
                                                         removeClientSubscription)
import           Data.Morpheus.Types                    (GQLRequest (..))
import           Data.Morpheus.Types.Internal.WebSocket (GQLClient (..), OutputAction (..))
import           Network.WebSockets                     (ServerApp, acceptRequestWith, forkPingThread, receiveData,
                                                         sendTextData)

-- | statefull GraphQL interpreter
type GQLAPI = GQLRequest -> IO (OutputAction IO ByteString)

handleGQLResponse :: GQLClient -> GQLState -> Int -> OutputAction IO ByteString -> IO ()
handleGQLResponse GQLClient {clientConnection = connection', clientID = clientId'} state sessionId' msg =
  case msg of
    PublishMutation { mutationChannels = channels'
                    , currentSubscriptionStateResolver = resolver'
                    , mutationResponse = response'
                    } -> sendTextData connection' response' >> publishUpdates channels' resolver' state
    InitSubscription {subscriptionQuery = selection', subscriptionChannels = channels'} ->
      addClientSubscription clientId' selection' channels' sessionId' state
    NoEffect response' -> sendTextData connection' response'

queryHandler :: GQLAPI -> GQLClient -> GQLState -> IO ()
queryHandler interpreter' client'@GQLClient {clientConnection = connection', clientID = id'} state =
  forever handleRequest
  where
    handleRequest = do
      msg <- receiveData connection'
      case parseApolloRequest msg of
        Left x -> print x
        Right ApolloSubscription {apolloType = "subscription_end", apolloId = Just sid'} ->
          removeClientSubscription id' sid' state
        Right ApolloSubscription { apolloType = "subscription_start"
                                 , apolloId = Just sid'
                                 , apolloQuery = Just query'
                                 , apolloOperationName = name'
                                 , apolloVariables = variables'
                                 } -> interpreter' request >>= handleGQLResponse client' state sid'
          where request = GQLRequest {query = query', operationName = name', variables = variables'}
        Right _ -> return ()

-- | Wai Websocket Server App for GraphQL subscriptions
gqlSocketApp :: GQLAPI -> GQLState -> ServerApp
gqlSocketApp interpreter' state pending = do
  connection' <- acceptRequestWith pending apolloProtocol
  forkPingThread connection' 30
  client' <- connectClient connection' state
  finally (queryHandler interpreter' client' state) (disconnectClient client' state)
