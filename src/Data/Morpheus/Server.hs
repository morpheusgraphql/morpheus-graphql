{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server
  ( gqlSocketApp
  , initGQLState
  , GQLState
  ) where

import           Control.Exception                      (finally)
import           Control.Monad                          (forever)
import           Data.Morpheus.Server.Apollo            (ApolloSubscription (..), apolloProtocol, parseApolloRequest)
import           Data.Morpheus.Server.ClientRegister    (GQLState, addClientSubscription, connectClient,
                                                         disconnectClient, initGQLState, publishUpdates,
                                                         removeClientSubscription)
import           Data.Morpheus.Types.Internal.WebSocket (GQLClient (..), OutputAction (..))
import           Data.Text                              (Text)
import           Network.WebSockets                     (ServerApp, acceptRequestWith, forkPingThread, receiveData,
                                                         sendTextData)

type GQLAPI = Text -> IO (OutputAction Text)

handleGQLResponse :: GQLClient -> GQLState -> Int -> OutputAction Text -> IO ()
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
        Right ApolloSubscription {apolloType = "subscription_start", apolloId = Just sid'} ->
          interpreter' msg >>= handleGQLResponse client' state sid'
        Right _ -> return ()

gqlSocketApp :: GQLAPI -> GQLState -> ServerApp
gqlSocketApp interpreter' state pending = do
  connection' <- acceptRequestWith pending apolloProtocol
  forkPingThread connection' 30
  client' <- connectClient connection' state
  finally (queryHandler interpreter' client' state) (disconnectClient client' state)
