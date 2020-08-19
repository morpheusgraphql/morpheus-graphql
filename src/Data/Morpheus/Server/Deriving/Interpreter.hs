{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Server.Deriving.Interpreter
  ( Interpreter (..),
    AppRunner (..),
  )
where

-- MORPHEUS
import Data.Morpheus.Core
  ( App (..),
    debugConfig,
    defaultConfig,
    runApp,
  )
import Data.Morpheus.Server.Deriving.Resolve
  ( RootResolverConstraint,
    coreResolver,
    stateless,
    statelessResolver,
  )
import Data.Morpheus.Types
  ( Event,
    RootResolver (..),
  )
import Data.Morpheus.Types.IO
  ( GQLRequest,
    GQLResponse,
    MapAPI (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResponseStream,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( Input,
    Stream,
    toOutStream,
  )

class Monad m => AppRunner e (m :: * -> *) a b where
  runWith :: ResponseStream e m (App e m) -> a -> b

instance Monad m => AppRunner e m GQLRequest (m GQLResponse) where
  runWith appF req = stateless $ do
    app <- appF
    runApp app req

instance Monad m => AppRunner (Event ch cont) m (Input api) (Stream api (Event ch cont) m) where
  runWith appF =
    toOutStream
      ( \req -> do
          app <- appF
          runApp app req
      )

instance (Monad m, MapAPI a a) => AppRunner e m a (m a) where
  runWith app = mapAPI (runWith app)

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
    Monad m =>
    (RootResolverConstraint m e query mut sub) =>
    RootResolver m e query mut sub ->
    a ->
    b
  debugInterpreter ::
    Monad m =>
    (RootResolverConstraint m e query mut sub) =>
    RootResolver m e query mut sub ->
    a ->
    b

instance Interpreter e m GQLRequest (m GQLResponse) where
  interpreter root = statelessResolver root defaultConfig
  debugInterpreter root = statelessResolver root debugConfig

instance Interpreter (Event ch cont) m (Input api) (Stream api (Event ch cont) m) where
  interpreter root = toOutStream (coreResolver root defaultConfig)
  debugInterpreter root = toOutStream (coreResolver root debugConfig)

instance (MapAPI a a) => Interpreter e m a (m a) where
  interpreter root = mapAPI (interpreter root)
  debugInterpreter root = mapAPI (debugInterpreter root)
