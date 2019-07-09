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
import           Data.Text                              (Text)
import           Network.WebSockets                     (ServerApp, acceptRequestWith, forkPingThread, pendingRequest,
                                                         receiveData, sendTextData)

-- MORPHEUS
import           Data.Morpheus.Resolve.Resolve          (RootResCon, streamResolver)
import           Data.Morpheus.Server.Apollo            (SubAction (..), acceptApolloSubProtocol, apolloFormat)
import           Data.Morpheus.Server.ClientRegister    (GQLState, addClientSubscription, connectClient,
                                                         disconnectClient, initGQLState, publishUpdates,
                                                         removeClientSubscription)
import           Data.Morpheus.Types.Internal.Stream    (ResponseEvent (..), ResponseStream, closeStream)
import           Data.Morpheus.Types.Internal.WebSocket (GQLClient (..))
import           Data.Morpheus.Types.Resolver           (GQLRootResolver (..))

handleGQLResponse :: Eq s => GQLClient IO s -> GQLState IO s -> Text -> ResponseStream IO s ByteString -> IO ()
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
  connection <- acceptRequestWith pending $ acceptApolloSubProtocol (pendingRequest pending)
  forkPingThread connection 30
  client <- connectClient connection state
  finally (queryHandler client) (disconnectClient client state)
  where
    queryHandler client@GQLClient {clientConnection, clientID} = forever handleRequest
      where
        handleRequest = receiveData clientConnection >>= resolveMessage . apolloFormat
          where
            resolveMessage (SubError x) = print x
            resolveMessage (AddSub sessionId request) =
              handleGQLResponse client state sessionId (encode <$> streamResolver gqlRoot request)
            resolveMessage (RemoveSub sessionId) = removeClientSubscription clientID sessionId state
