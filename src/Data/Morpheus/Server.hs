{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
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
import           Network.WebSockets             ( ServerApp
                                                , acceptRequestWith
                                                , pendingRequest
                                                , receiveData
                                                , withPingThread
                                                )

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , coreResolver
                                                )
import           Data.Morpheus.Types.Internal.Apollo
                                                ( acceptApolloSubProtocol )
import           Data.Morpheus.Execution.Server.Subscription
                                                ( GQLState
                                                , connect
                                                , disconnect
                                                , initGQLState
                                                , Stream(..)
                                                , execStream
                                                , concatStream
                                                , initApolloStream
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..) )
import           Data.Morpheus.Types.Internal.WebSocket
                                                ( GQLClient(..) )


subscriptionHandler 
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub 
  -> Stream m e
  -> m (Stream m e)
subscriptionHandler root s = 
      traverse getInput (active s)
      >>= fmap concatStream . traverse (initApolloStream (coreResolver root) s) 
  where
    getInput client = liftIO (receiveData (clientConnection client))

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub
  -> GQLState m e
  -> (m (Stream m e) -> IO (Stream m e))
  -> ServerApp
gqlSocketMonadIOApp root state f pending = do
  connection <- acceptRequestWith pending
    $ acceptApolloSubProtocol (pendingRequest pending)
  withPingThread connection 30 (return ()) $ do
      iStrem <- connect connection
      s <- f (execStream iStrem state)
      finally 
        (queryHandler s >> pure ()) 
        $ f (execStream (disconnect s) state) 
        >> pure ()
 where
  queryHandler st
        = f
        $ forever
        $ subscriptionHandler root st
        >>= (`execStream` state)
          
-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState IO e
  -> ServerApp
gqlSocketApp gqlRoot state = gqlSocketMonadIOApp gqlRoot state id
