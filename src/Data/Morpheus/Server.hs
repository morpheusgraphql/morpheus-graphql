{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE CPP                  #-}

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
                                                , Connection
                                                )
import qualified Network.WebSockets          as WS

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
                                                , toResponseStream
                                                , runStream
                                                , traverseS
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..) )




-- fix breaking changes   
wsPing :: Connection -> IO () -> IO ()
#if MIN_VERSION_template_haskell(0,12,6)
wsPing connection = WS.withPingThread connection 30 (return ())
#else
wsPing connection = (WS.forkPingThread connection 30 >>)
#endif

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub
  -> GQLState Connection e m
  -> (m () -> IO ())
  -> ServerApp
gqlSocketMonadIOApp root state f pending = do
  connection <- acceptApolloRequest pending
  wsPing connection $ do
      stream <- connect connection
      finally
        (queryHandler stream) 
        $ f (runStream (disconnect stream) state) 
 where
  queryHandler st
        = f
        $ forever
        $ traverseS (toResponseStream  (coreResolver root)) st
        >>= (`runStream` state)

-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState Connection e IO
  -> ServerApp
gqlSocketApp gqlRoot state = gqlSocketMonadIOApp gqlRoot state id
