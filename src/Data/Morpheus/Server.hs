{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
import           Data.Morpheus.Resolve.Resolve          (RootResCon, resolveStream)
import           Data.Morpheus.Server.Apollo            (ApolloSubscription (..), apolloProtocol, parseApolloRequest)
import           Data.Morpheus.Server.ClientRegister    (GQLState, addClientSubscription, connectClient,
                                                         disconnectClient, initGQLState, publishUpdates,
                                                         removeClientSubscription)
import           Data.Morpheus.Types                    (GQLRequest (..))
import           Data.Morpheus.Types.Internal.WebSocket (GQLClient (..), OutputAction (..))
import           Data.Morpheus.Types.Resolver           (GQLRootResolver (..))
import           Network.WebSockets                     (ServerApp, acceptRequestWith, forkPingThread, receiveData,
                                                         sendTextData)

handleGQLResponse :: Eq s => GQLClient IO s -> GQLState IO s -> Int -> OutputAction IO s ByteString -> IO ()
handleGQLResponse GQLClient {clientConnection, clientID} state sessionId' msg =
  case msg of
    PublishMutation {mutationChannels, mutationResponse} ->
      sendTextData clientConnection mutationResponse >> publishUpdates mutationChannels state
    InitSubscription wsSubscription -> addClientSubscription clientID wsSubscription sessionId' state
    NoAction response' -> sendTextData clientConnection response'

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketApp :: RootResCon IO s a b c => GQLRootResolver IO s a b c -> GQLState IO s -> ServerApp
gqlSocketApp gqlRoot state pending = do
  connection' <- acceptRequestWith pending apolloProtocol
  forkPingThread connection' 30
  client' <- connectClient connection' state
  finally (queryHandler client') (disconnectClient client' state)
  where
    queryHandler client@GQLClient {clientConnection, clientID} = forever handleRequest
      where
        handleRequest = receiveData clientConnection >>= resolveMessage . parseApolloRequest
          where
            resolveMessage (Left x) = print x
            resolveMessage (Right ApolloSubscription {apolloType = "subscription_end", apolloId = Just sid'}) =
              removeClientSubscription clientID sid' state
            resolveMessage (Right ApolloSubscription { apolloType = "subscription_start"
                                                     , apolloId = Just sessionId
                                                     , apolloQuery = Just query
                                                     , apolloOperationName = operationName
                                                     , apolloVariables = variables
                                                     }) = do
              value <- resolveStream gqlRoot $ GQLRequest {query, operationName, variables}
              handleGQLResponse client state sessionId (encode <$> value)
            resolveMessage (Right _) = return ()
