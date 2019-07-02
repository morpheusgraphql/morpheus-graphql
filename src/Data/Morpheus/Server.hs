{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , initGQLState
  , GQLState
  ) where

import           Control.Exception                      (finally)
import           Control.Monad                          (forever)
import           Data.Aeson                             (encode)
import           Data.ByteString.Lazy.Char8             (ByteString)
import           Data.Morpheus.Resolve.Operator         (RootResCon)
import           Data.Morpheus.Resolve.Resolve          (resolveStream)
import           Data.Morpheus.Server.Apollo            (ApolloSubscription (..), apolloProtocol, parseApolloRequest)
import           Data.Morpheus.Server.ClientRegister    (GQLState, addClientSubscription, connectClient,
                                                         disconnectClient, initGQLState, publishUpdates,
                                                         removeClientSubscription)
import           Data.Morpheus.Types                    (GQLRequest (..))
import           Data.Morpheus.Types.Internal.WebSocket (GQLClient (..), OutputAction (..))
import           Data.Morpheus.Types.Resolver           (GQLRootResolver (..))
import           Data.Text                              (pack)
import           Data.Typeable                          (Typeable)
import           Network.WebSockets                     (ServerApp, acceptRequestWith, forkPingThread, receiveData,
                                                         sendTextData)

type ChannelCon s = Show s

handleGQLResponse :: ChannelCon s => GQLClient -> GQLState -> Int -> OutputAction IO s ByteString -> IO ()
handleGQLResponse GQLClient {clientConnection = connection', clientID = clientId'} state sessionId' msg =
  case msg of
    PublishMutation { mutationChannels = channels'
                    , currentSubscriptionStateResolver = resolver'
                    , mutationResponse = response'
                    } -> sendTextData connection' response' >> publishUpdates channels' resolver' state
    InitSubscription {subscriptionQuery = selection', subscriptionChannels = channels'} ->
      addClientSubscription clientId' selection' (toIds channels') sessionId' state
    NoAction response' -> sendTextData connection' response'
  where
    toIds = map (pack . show)

queryHandler ::
     (Typeable s, Show s, RootResCon IO s a b c) => GQLRootResolver IO s a b c -> GQLClient -> GQLState -> IO ()
queryHandler gqlRoot client'@GQLClient {clientConnection = connection', clientID = id'} state = forever handleRequest
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
                                 } -> do
          value <- resolveStream gqlRoot request
          handleGQLResponse client' state sid' (encode <$> value)
          where request = GQLRequest {query = query', operationName = name', variables = variables'}
        Right _ -> return ()

-- | Wai Websocket Server App for GraphQL subscriptions
gqlSocketApp :: (Typeable s, Show s, RootResCon IO s a b c) => GQLRootResolver IO s a b c -> GQLState -> ServerApp
gqlSocketApp gqlRoot state pending = do
  connection' <- acceptRequestWith pending apolloProtocol
  forkPingThread connection' 30
  client' <- connectClient connection' state
  finally (queryHandler gqlRoot client' state) (disconnectClient client' state)
