{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Morpheus.Execution.Server.Interpreter
  ( Interpreter(..)
  )
where

import           Data.Aeson                     ( encode )
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

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , byteStringIO
                                                , statefulResolver
                                                , statelessResolver
                                                , streamResolver
                                                , coreResolver
                                                )
import           Data.Morpheus.Execution.Subscription.ClientRegister
                                                ( GQLState )
import           Data.Morpheus.Types.Internal.Resolver
                                                ( GQLRootResolver(..) )
import           Data.Morpheus.Types.Internal.Stream
                                                ( ResponseStream )
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
class Interpreter k m e  where
  interpreter ::
       Monad m
    => (RootResCon m e query mut sub) =>
         GQLRootResolver m e query mut sub -> k

{-
  simple HTTP stateless Interpreter without side effects
-}
type StateLess m a = a -> m a

instance Interpreter (GQLRequest -> m GQLResponse) m e   where
  interpreter = statelessResolver

instance Interpreter (StateLess m LB.ByteString) m e  where
  interpreter root = byteStringIO (statelessResolver root)

instance Interpreter (StateLess m LT.Text) m e  where
  interpreter root request =
    decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (StateLess m ByteString) m e  where
  interpreter root request =
    LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (StateLess m Text) m e  where
  interpreter root request =
    LT.toStrict <$> interpreter root (LT.fromStrict request)

{-
   HTTP Interpreter with state and side effects, every mutation will
   trigger subscriptions in  shared `GQLState`
-}
type WSPub m e a = GQLState m e -> a -> m a

instance Interpreter (WSPub IO e LB.ByteString) IO e where
  interpreter root state = statefulResolver state (coreResolver root)

instance Interpreter (WSPub IO e  LT.Text) IO e where
  interpreter root state request =
    decodeUtf8 <$> interpreter root state (encodeUtf8 request)

instance Interpreter (WSPub IO e  ByteString) IO e where
  interpreter root state request =
    LB.toStrict <$> interpreter root state (LB.fromStrict request)

instance Interpreter (WSPub IO e  Text) IO e  where
  interpreter root state request =
    LT.toStrict <$> interpreter root state (LT.fromStrict request)

{-
   WebSocket Interpreter without state and side effects, mutations and subscription will return Actions
   that will be executed in WebSocket server
-}
type WSSub m e a = a -> ResponseStream m e a

instance Interpreter (GQLRequest -> ResponseStream m e  LB.ByteString) e m where
  interpreter root request = encode <$> streamResolver root request

instance Interpreter (WSSub m e  LB.ByteString)  e m where
  interpreter root = byteStringIO (streamResolver root)

instance Interpreter (WSSub m e  LT.Text)  e m  where
  interpreter root request =
    decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter (WSSub m e ByteString) e m  where
  interpreter root request =
    LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter (WSSub m e Text) e m where
  interpreter root request =
    LT.toStrict <$> interpreter root (LT.fromStrict request)
