{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Morpheus.Interpreter
  ( Interpreter(..)
  ) where

import           Data.Aeson                          (encode)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy.Char8          as LB (ByteString, fromStrict, toStrict)
import           Data.Text                           (Text)
import qualified Data.Text.Lazy                      as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding             (decodeUtf8, encodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Resolve.Resolve       (RootResCon, byteStringIO, statefulResolver, statelessResolver,
                                                      streamResolver)
import           Data.Morpheus.Server.ClientRegister (GQLState)
import           Data.Morpheus.Types.Internal.Stream (ResponseStream)
import           Data.Morpheus.Types.IO              (GQLRequest, GQLResponse)
import           Data.Morpheus.Types.Resolver        (GQLRootResolver (..))

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
class Interpreter k m s where
  interpreter ::
       Monad m
    => (RootResCon m s a b c) =>
         GQLRootResolver m s a b c -> k

{-
  simple HTTP stateless Interpreter without side effects
-}
type StateLess m a = a -> m a

instance Interpreter (GQLRequest -> m GQLResponse) m s where
  interpreter = statelessResolver

instance Interpreter (StateLess m LB.ByteString) m s where
  interpreter root = byteStringIO (statelessResolver root)

instance Interpreter (StateLess m LT.Text) m s where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (StateLess m ByteString) m s where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (StateLess m Text) m s where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)

{-
   HTTP Interpreter with state and side effects, every mutation will
   trigger subscriptions in  shared `GQLState`
-}
type WSPub m s a = GQLState m s -> a -> m a

instance Interpreter (WSPub IO s LB.ByteString) IO s where
  interpreter root state = statefulResolver state (byteStringIO (streamResolver root))

instance Interpreter (WSPub IO s LT.Text) IO s where
  interpreter root state request = decodeUtf8 <$> interpreter root state (encodeUtf8 request)

instance Interpreter (WSPub IO s ByteString) IO s where
  interpreter root state request = LB.toStrict <$> interpreter root state (LB.fromStrict request)

instance Interpreter (WSPub IO s Text) IO s where
  interpreter root state request = LT.toStrict <$> interpreter root state (LT.fromStrict request)

{-
   WebSocket Interpreter without state and side effects, mutations and subscription will return Actions
   that will be executed in WebSocket server
-}
type WSSub m s a = a -> ResponseStream m s a

instance Interpreter (GQLRequest -> ResponseStream m s LB.ByteString) m s where
  interpreter root request = encode <$> streamResolver root request

instance Interpreter (WSSub m s LB.ByteString) m s where
  interpreter root = byteStringIO (streamResolver root)

instance Interpreter (WSSub m s LT.Text) m s where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (WSSub m s ByteString) m s where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (WSSub m s Text) m s where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)
