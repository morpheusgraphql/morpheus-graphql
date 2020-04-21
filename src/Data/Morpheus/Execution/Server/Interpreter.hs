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
                                                , MapAPI(..)
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
        -> b

instance Interpreter e m GQLRequest (m GQLResponse) where
  interpreter = statelessResolver

instance Interpreter e m Input (Stream e m) where
  interpreter root = toOutStream (coreResolver root)

instance ( MapAPI a ) => 
  Interpreter e m a (m a)  where
    interpreter root = mapAPI (interpreter root)
