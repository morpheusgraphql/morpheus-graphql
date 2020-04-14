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

import           Data.Foldable                  ( traverse_ )
import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Text                      ( Text )
import           Network.WebSockets             ( ServerApp
                                                , acceptRequestWith
                                                , pendingRequest
                                                , receiveData
                                                , sendTextData
                                                , withPingThread
                                                )

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , coreResolver
                                                )
import           Data.Morpheus.Types.Internal.Apollo
                                                ( SubAction(..)
                                                , acceptApolloSubProtocol
                                                , apolloFormat
                                                , toApolloResponse
                                                )
import           Data.Morpheus.Execution.Server.Subscription
                                                ( GQLState
                                                , connectClient
                                                , disconnectClient
                                                , initGQLState
                                                , publishEvent
                                                , startSubscription
                                                , endSubscription
                                                , Stream(..)
                                                , Action(..)
                                                , handleSubscription
                                                , runStream
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
                                                ( ValidValue )


apolloToAction 
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub 
  -> GQLClient m e 
  -> SubAction  
  -> Stream m e 
apolloToAction _ _ (SubError x) = Stream [Log x]
apolloToAction root client (AddSub sessionId request) 
  = handleSubscription client sessionId (coreResolver root request)
apolloToAction _ client (RemoveSub sessionId) 
  = endSubscription (clientID client) sessionId 

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub
  -> GQLState m e
  -> (m () -> IO ())
  -> ServerApp
gqlSocketMonadIOApp root state f pending = do
  connection <- acceptRequestWith pending
    $ acceptApolloSubProtocol (pendingRequest pending)
  withPingThread connection 30 (return ()) $ do
      client <- connectClient connection state
      finally (f $ queryHandler client) (runStream (disconnectClient client) state)
 where
  queryHandler client = forever handleRequest
   where
    handleRequest = do
      d <- liftIO $ receiveData (clientConnection client)
      runStream (apolloToAction root client (apolloFormat d)) state

-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState IO e
  -> ServerApp
gqlSocketApp gqlRoot state = gqlSocketMonadIOApp gqlRoot state id
