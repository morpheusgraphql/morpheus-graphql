{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
import           Data.Morpheus.Resolve.Resolve          (RootResCon, streamResolver)
import           Data.Morpheus.Server.Apollo            (ApolloAction (..), apolloProtocol, oldApolloFormat)
import           Data.Morpheus.Server.ClientRegister    (GQLState, addClientSubscription, connectClient,
                                                         disconnectClient, initGQLState, publishUpdates,
                                                         removeClientSubscription)
import           Data.Morpheus.Types.Internal.Stream    (ResponseEvent (..), ResponseStream, closeStream)
import           Data.Morpheus.Types.Internal.WebSocket (GQLClient (..))
import           Data.Morpheus.Types.Resolver           (GQLRootResolver (..))
import           Network.WebSockets                     (ServerApp, acceptRequestWith, forkPingThread, receiveData,
                                                         sendTextData)

handleGQLResponse :: Eq s => GQLClient IO s -> GQLState IO s -> Int -> ResponseStream IO s ByteString -> IO ()
handleGQLResponse GQLClient {clientConnection, clientID} state sessionId stream = do
  (actions, response) <- closeStream stream
  sendTextData clientConnection response
  mapM_ execute actions
  where
    execute (Publish pub)   = publishUpdates state pub
    execute (Subscribe sub) = addClientSubscription clientID sub sessionId state

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
        handleRequest = receiveData clientConnection >>= resolveMessage . oldApolloFormat
          where
            resolveMessage (ApolloError x) = print x
            resolveMessage (ApolloRemove sessionId) = removeClientSubscription clientID sessionId state
            resolveMessage (ApolloRequest sessionId request) =
              handleGQLResponse client state sessionId (encode <$> streamResolver gqlRoot request)
            resolveMessage ApolloNoAction = return ()
