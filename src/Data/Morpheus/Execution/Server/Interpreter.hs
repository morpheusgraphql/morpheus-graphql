{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Morpheus.Execution.Server.Interpreter
  ( Interpreter(..)
  )
where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as LB
                                                ( ByteString
                                                , fromStrict
                                                , toStrict
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as LT
                                                ( Text
                                                , fromStrict
                                                , toStrict
                                                )
import           Data.Text.Lazy.Encoding        ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Control.Monad.IO.Class         ( MonadIO() )

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Subscription
                                                ( Action
                                                , Stream
                                                , Input
                                                , toOutStream
                                                )
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , byteStringIO
                                                , coreResolver
                                                , statelessResolver
                                                )
import           Data.Morpheus.Server           ( statefull
                                                , GQLState
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..)
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest
                                                , GQLResponse
                                                )

-- | main query processor and resolver
--  possible versions of interpreter
--
-- 1. with effect and state: where 'GQLState' is State Monad of subscriptions
--
--     @
--      k :: GQLState -> a -> IO a
--     @
--
-- 2. without effect and state: stateless query processor without any effect,
--    if you don't need any subscription use this one , is simple and fast
--
--     @
--       k :: a -> IO a
--       -- or
--       k :: GQLRequest -> IO GQLResponse
--     @
class Interpreter e m a b where
  interpreter ::
       Monad m
    => (RootResCon m e query mut sub) 
        => GQLRootResolver m e query mut sub
        -> a
        -> m b
  interpreterWS ::
       MonadIO m
    => (RootResCon m e query mut sub) 
        => GQLRootResolver m e query mut sub
        -> GQLState e m 
        -> a
        -> m b

instance Interpreter e m GQLRequest GQLResponse where
  interpreter = statelessResolver
  -- interpreterWS root state =  statefulResolver state root 

instance Interpreter e m Input (Stream ref e m) where
  interpreter root = pure . toOutStream (coreResolver root)

instance Interpreter e m LB.ByteString LB.ByteString  where
  interpreter root = byteStringIO (interpreter root)
  -- interpreterWS root state = byteStringIO (statefulResolver state root) 

instance Interpreter e m LT.Text LT.Text  where
  interpreter root 
    = fmap decodeUtf8 . interpreter root . encodeUtf8
  interpreterWS root state 
    = fmap decodeUtf8 . interpreterWS root state . encodeUtf8

instance Interpreter e m ByteString ByteString where
  interpreter root 
    = fmap LB.toStrict . interpreter root . LB.fromStrict
  interpreterWS root state  
    = fmap LB.toStrict . interpreterWS root state . LB.fromStrict

instance Interpreter e m Text Text where
  interpreter root 
    = fmap LT.toStrict . interpreter root . LT.fromStrict
  interpreterWS root state 
    = fmap LT.toStrict . interpreterWS root state  . LT.fromStrict