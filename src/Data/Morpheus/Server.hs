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
  )
where

import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Data.Text                      ( Text )
import           Network.WebSockets             ( ServerApp
                                                , acceptRequestWith
                                                , forkPingThread
                                                , pendingRequest
                                                , receiveData
                                                , sendTextData
                                                )

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , coreResolver
                                                )
import           Data.Morpheus.Execution.Subscription.Apollo
                                                ( SubAction(..)
                                                , acceptApolloSubProtocol
                                                , apolloFormat
                                                , toApolloResponse
                                                )
import           Data.Morpheus.Execution.Subscription.ClientRegister
                                                ( GQLState
                                                , addClientSubscription
                                                , connectClient
                                                , disconnectClient
                                                , initGQLState
                                                , publishUpdates
                                                , removeClientSubscription
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..)
                                                , GQLChannel(..)
                                                , ResponseEvent(..)
                                                , ResponseStream
                                                , runResultT
                                                , Result(..)
                                                )
import           Data.Morpheus.Types.Internal.WebSocket
                                                ( GQLClient(..) )
import           Data.Morpheus.Types.IO         ( GQLResponse(..) )
import           Data.Morpheus.Types.Internal.AST
                                                ( Value )

handleSubscription
  :: (Eq (StreamChannel e), GQLChannel e)
  => GQLClient IO e
  -> GQLState IO e
  -> Text
  -> ResponseStream e IO Value
  -> IO ()
handleSubscription GQLClient { clientConnection, clientID } state sessionId stream
  = do
    response <- runResultT stream
    case response of
      Success { events } -> mapM_ execute events
      Failure errors     -> sendTextData
        clientConnection
        (toApolloResponse sessionId $ Errors errors)
 where
  execute (Publish   pub) = publishUpdates state pub
  execute (Subscribe sub) = addClientSubscription clientID sub sessionId state

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketApp
  :: RootResCon IO e que mut sub
  => GQLRootResolver IO e que mut sub
  -> GQLState IO e
  -> ServerApp
gqlSocketApp gqlRoot state pending = do
  connection <- acceptRequestWith pending
    $ acceptApolloSubProtocol (pendingRequest pending)
  forkPingThread connection 30
  client <- connectClient connection state
  finally (queryHandler client) (disconnectClient client state)
 where
  queryHandler client = forever handleRequest
   where
    handleRequest =
      receiveData (clientConnection client) >>= resolveMessage . apolloFormat
     where
      resolveMessage (SubError x) = print x
      resolveMessage (AddSub sessionId request) =
        handleSubscription client state sessionId (coreResolver gqlRoot request)
      resolveMessage (RemoveSub sessionId) =
        removeClientSubscription (clientID client) sessionId state
