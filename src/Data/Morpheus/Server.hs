{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , gqlSocketMonadIOApp
  , initGQLState
  , GQLState
  )
where

import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
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
  :: (Eq (StreamChannel e), GQLChannel e, MonadIO m)
  => GQLClient m e
  -> GQLState m e
  -> Text
  -> ResponseStream e m Value
  -> m ()
handleSubscription GQLClient { clientConnection, clientID } state sessionId stream
  = do
    response <- runResultT stream
    case response of
      Success { events } -> mapM_ execute events
      Failure errors     -> liftIO $ sendTextData
        clientConnection
        (toApolloResponse sessionId $ Errors errors)
 where
  execute (Publish   pub) = publishUpdates state pub
  execute (Subscribe sub) = addClientSubscription clientID sub sessionId state

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub
  -> GQLState m e
  -> (m () -> IO ())
  -> ServerApp
gqlSocketMonadIOApp gqlRoot state f pending = do
  connection <- acceptRequestWith pending
    $ acceptApolloSubProtocol (pendingRequest pending)
  forkPingThread connection 30
  client <- connectClient connection state
  finally (f $ queryHandler client) (disconnectClient client state)
 where
  queryHandler client = forever handleRequest
   where
    handleRequest = do
      d <- liftIO $ receiveData (clientConnection client)
      resolveMessage (apolloFormat d)
     where
      resolveMessage (SubError x) = liftIO $ print x
      resolveMessage (AddSub sessionId request) =
        handleSubscription client state sessionId (coreResolver gqlRoot request)
      resolveMessage (RemoveSub sessionId) =
        removeClientSubscription (clientID client) sessionId state

-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState IO e
  -> ServerApp
gqlSocketApp gqlRoot state pending = gqlSocketMonadIOApp gqlRoot state id pending
