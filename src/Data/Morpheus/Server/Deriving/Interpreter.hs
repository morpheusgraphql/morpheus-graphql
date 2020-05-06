{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Server.Deriving.Interpreter
  ( Interpreter (..),
  )
where

-- MORPHEUS

import Data.Morpheus.Server.Deriving.Resolve
  ( RootResCon,
    coreResolver,
    statelessResolver,
  )
import Data.Morpheus.Types
  ( GQLRootResolver (..),
  )
import Data.Morpheus.Types.IO
  ( GQLRequest,
    GQLResponse,
    MapAPI (..),
  )
import Data.Morpheus.Types.Internal.Subscription
  ( Input,
    Stream,
    toOutStream,
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
    Monad m =>
    (RootResCon m e query mut sub) =>
    GQLRootResolver m e query mut sub ->
    a ->
    b

instance Interpreter e m GQLRequest (m GQLResponse) where
  interpreter = statelessResolver

instance Interpreter e m (Input api) (Stream api e m) where
  interpreter root = toOutStream (coreResolver root)

instance
  (MapAPI a) =>
  Interpreter e m a (m a)
  where
  interpreter root = mapAPI (interpreter root)
