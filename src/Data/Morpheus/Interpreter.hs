{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Morpheus.Interpreter
  ( Interpreter(..)
  ) where

import           Data.Aeson                             (encode)
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Lazy.Char8             as LB (ByteString, fromStrict, toStrict)
import           Data.Morpheus.Resolve.Resolve          (packStream, resolve, resolveByteString, resolveStream,
                                                         resolveStreamByteString)
import           Data.Morpheus.Server.ClientRegister    (GQLState)
import           Data.Morpheus.Types.GQLOperator        (GQLMutation (..), GQLQuery (..), GQLSubscription (..))
import           Data.Morpheus.Types.Internal.WebSocket (OutputAction)
import           Data.Morpheus.Types.Request            (GQLRequest)
import           Data.Morpheus.Types.Response           (GQLResponse)
import           Data.Morpheus.Types.Types              (GQLRootResolver (..))
import           Data.Text                              (Text)
import qualified Data.Text.Lazy                         as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                (decodeUtf8, encodeUtf8)

-- | main query processor and resolver
class Interpreter k
  -- | possible versions of interpreter
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
  where
  interpreter :: (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRootResolver q m s -> k

{-
  simple HTTP stateless Interpreter without side effects
-}
type StateLess a = a -> IO a

instance Interpreter (GQLRequest -> IO GQLResponse) where
  interpreter = resolve

instance Interpreter (StateLess LB.ByteString) where
  interpreter = resolveByteString

instance Interpreter (StateLess LT.Text) where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (StateLess ByteString) where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (StateLess Text) where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)

{-
   HTTP Interpreter with state and side effects, every mutation will
   trigger subscriptions in  shared `GQLState`
-}
type WSPub a = GQLState -> a -> IO a

instance Interpreter (WSPub LB.ByteString) where
  interpreter root state = packStream state (resolveStreamByteString root)

instance Interpreter (WSPub LT.Text) where
  interpreter root state request = decodeUtf8 <$> interpreter root state (encodeUtf8 request)

instance Interpreter (WSPub ByteString) where
  interpreter root state request = LB.toStrict <$> interpreter root state (LB.fromStrict request)

instance Interpreter (WSPub Text) where
  interpreter root state request = LT.toStrict <$> interpreter root state (LT.fromStrict request)

{-
   Websocket Interpreter without state and side effects, mutations and subscription will return Actions
   that will be executed in Websocket server
-}
type WSSub a = a -> IO (OutputAction a)

instance Interpreter (GQLRequest -> IO (OutputAction LB.ByteString)) where
  interpreter root request = fmap encode <$> resolveStream root request

instance Interpreter (WSSub LB.ByteString) where
  interpreter = resolveStreamByteString

instance Interpreter (WSSub LT.Text) where
  interpreter root request = fmap decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (WSSub ByteString) where
  interpreter root request = fmap LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (WSSub Text) where
  interpreter root request = fmap LT.toStrict <$> interpreter root (LT.fromStrict request)
