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

--import           Data.Morpheus.Types.Internal.Value  (Value)
import           Data.Morpheus.Types.Types              (GQLRoot (..))
import           Data.Text                              (Text)
import qualified Data.Text.Lazy                         as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                (decodeUtf8, encodeUtf8)

type WSSub a = InputAction a -> IO (OutputAction a)

type WSPub a = GQLState -> a -> IO a

type PureGQL a = a -> IO a

class Interpreter k where
  interpreter :: (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> k

instance Interpreter (WSSub Text) where
  interpreter = resolveStream

--instance Interpreter (WSSub Value) where
--  interpreter root state = packStream state (streamInterpreter root)
instance Interpreter (WSPub LB.ByteString) where
  interpreter root state = packStream state (resolveStream root)

instance Interpreter (PureGQL LB.ByteString) where
  interpreter root request = encode <$> resolve root request

instance Interpreter (PureGQL LT.Text) where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (PureGQL ByteString) where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (PureGQL Text) where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)
