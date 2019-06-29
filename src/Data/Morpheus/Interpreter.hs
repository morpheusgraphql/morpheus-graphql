{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Morpheus.Interpreter
  ( Interpreter(..)
  ) where

import           Data.Aeson                             (encode)
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Lazy.Char8             as LB (ByteString, fromStrict, toStrict)
import           Data.Morpheus.Resolve.Operator         (RootResCon)
import           Data.Morpheus.Resolve.Resolve          (packStream, resolve, resolveByteString, resolveStream,
                                                         resolveStreamByteString)
import           Data.Morpheus.Server.ClientRegister    (GQLState)
import           Data.Morpheus.Types.Internal.WebSocket (OutputAction)
import           Data.Morpheus.Types.IO                 (GQLRequest, GQLResponse)
import           Data.Morpheus.Types.Resolver           (GQLRootResolver (..))
import           Data.Text                              (Text)
import qualified Data.Text.Lazy                         as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                (decodeUtf8, encodeUtf8)

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
class Interpreter k m where
  interpreter ::
       Monad m
    => (RootResCon m a b c) =>
         GQLRootResolver m a b c -> k

{-
  simple HTTP stateless Interpreter without side effects
-}
type StateLess m a = a -> m a

instance Interpreter (GQLRequest -> m GQLResponse) m where
  interpreter = resolve

instance Interpreter (StateLess m LB.ByteString) m where
  interpreter = resolveByteString

instance Interpreter (StateLess m LT.Text) m where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (StateLess m ByteString) m where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (StateLess m Text) m where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)

{-
   HTTP Interpreter with state and side effects, every mutation will
   trigger subscriptions in  shared `GQLState`
-}
type WSPub m a = GQLState -> a -> m a

instance Interpreter (WSPub IO LB.ByteString) IO where
  interpreter root state = packStream state (resolveStreamByteString root)

instance Interpreter (WSPub IO LT.Text) IO where
  interpreter root state request = decodeUtf8 <$> interpreter root state (encodeUtf8 request)

instance Interpreter (WSPub IO ByteString) IO where
  interpreter root state request = LB.toStrict <$> interpreter root state (LB.fromStrict request)

instance Interpreter (WSPub IO Text) IO where
  interpreter root state request = LT.toStrict <$> interpreter root state (LT.fromStrict request)

{-
   Websocket Interpreter without state and side effects, mutations and subscription will return Actions
   that will be executed in Websocket server
-}
type WSSub m a = a -> m (OutputAction m a)

instance Interpreter (GQLRequest -> m (OutputAction m LB.ByteString)) m where
  interpreter root request = fmap encode <$> resolveStream root request

instance Interpreter (WSSub m LB.ByteString) m where
  interpreter = resolveStreamByteString

instance Interpreter (WSSub m LT.Text) m where
  interpreter root request = fmap decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (WSSub m ByteString) m where
  interpreter root request = fmap LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (WSSub m Text) m where
  interpreter root request = fmap LT.toStrict <$> interpreter root (LT.fromStrict request)
