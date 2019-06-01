{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Morpheus.Interpreter
  ( Interpreter(..)
  ) where

import           Data.Aeson                             (encode)
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Lazy.Char8             as LB (ByteString, fromStrict, toStrict)
import           Data.Morpheus.Resolve.Resolve          (packStream, resolve, resolveStream)
import           Data.Morpheus.Server.ClientRegister    (GQLState)
import           Data.Morpheus.Types.GQLOperator        (GQLMutation (..), GQLQuery (..), GQLSubscription (..))
import           Data.Morpheus.Types.Internal.WebSocket (InputAction, OutputAction)
import           Data.Morpheus.Types.Types              (GQLRoot (..))
import           Data.Text                              (Text)
import qualified Data.Text.Lazy                         as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                (decodeUtf8, encodeUtf8)

class Interpreter k where
  interpreter :: (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> k

{-
  simple HTTP Interpreter without subscriptions and side effects
-}
type PureGQL a = a -> IO a

instance Interpreter (PureGQL LB.ByteString) where
  interpreter root request = encode <$> resolve root request

instance Interpreter (PureGQL LT.Text) where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (PureGQL ByteString) where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (PureGQL Text) where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)

{-
   HTTP Interpreter with side effects, every mutation will
   trigger subscriptions in  shared `GQLState`
-}
type WSPub a = GQLState -> a -> IO a

instance Interpreter (WSPub LB.ByteString) where
  interpreter root state = packStream state (resolveStream root)

instance Interpreter (WSPub LT.Text) where
  interpreter root state request = decodeUtf8 <$> interpreter root state (encodeUtf8 request)

instance Interpreter (WSPub ByteString) where
  interpreter root state request = LB.toStrict <$> interpreter root state (LB.fromStrict request)

instance Interpreter (WSPub Text) where
  interpreter root state request = LT.toStrict <$> interpreter root state (LT.fromStrict request)

{-
   Websocket Interpreter with side effects, mutations and subscription will return Actions
   that will be executed in Websocket server
-}
type WSSub a = InputAction a -> IO (OutputAction a)

instance Interpreter (WSSub Text) where
  interpreter = resolveStream
-- TODO: instance Interpreter (WSSub Value) where
