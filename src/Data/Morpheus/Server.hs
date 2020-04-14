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
import           Control.Monad.IO.Class         ( MonadIO )
import           Network.WebSockets             ( ServerApp
                                                , withPingThread
                                                , Connection
                                                )

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , coreResolver
                                                )
import           Data.Morpheus.Types.Internal.Apollo
                                                ( acceptApolloRequest )
import           Data.Morpheus.Execution.Server.Subscription
                                                ( GQLState
                                                , connect
                                                , disconnect
                                                , initGQLState
                                                , Stream(..)
                                                , initApolloStream
                                                , runInput
                                                , runStream
                                                , IN
                                                , mapS
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..) )


-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub
  -> GQLState Connection e m
  -> (m () -> IO ())
  -> (m (Stream IN Connection e m) -> IO (Stream IN Connection e m))
  -> ServerApp
gqlSocketMonadIOApp root state f fIn pending = do
  connection <- acceptApolloRequest pending
  withPingThread connection 30 (return ()) $ do
      iStrem <- connect connection
      s <- fIn (mapS (runInput state) iStrem)
      finally
        (queryHandler s) 
        $ f (runStream (disconnect s) state) 
 where
  queryHandler st
        = f
        $ forever
        $ mapS (initApolloStream  (coreResolver root)) st
        >>= (`runStream` state)

-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState Connection e IO
  -> ServerApp
gqlSocketApp gqlRoot state = gqlSocketMonadIOApp gqlRoot state id id
