{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | GQL Types
module Data.Morpheus.Types
  ( Event (..),
    GQLType (KIND, description),
    GQLScalar (parseValue, serialize),
    GQLRequest (..),
    GQLResponse (..),
    ID (..),
    ScalarValue (..),
    GQLRootResolver (..),
    constRes,
    constMutRes,
    Undefined (..),
    Resolver,
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    lift,
    liftEither,
    failRes,
    WithOperation,
    publish,
    subscribe,
    unsafeInternalContext,
    SubField,
    Input,
    Stream,
    WS,
    HTTP,
    -- Resolvers
    SmartResolver,
    ComposedResolver,
    ResolverQ,
    ResolverM,
    ResolverS,
    -- Resolvers Deprecated
    ResolveQ,
    ResolveM,
    ResolveS,
    Res,
    MutRes,
    SubRes,
    IORes,
    IOMutRes,
    IOSubRes,
  )
where

import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Either (either)
-- MORPHEUS
import Data.Morpheus.Server.Types.GQLScalar
  ( GQLScalar
      ( parseValue,
        serialize
      ),
  )
import Data.Morpheus.Server.Types.GQLType (GQLType (KIND, description))
import Data.Morpheus.Server.Types.ID (ID (..))
import Data.Morpheus.Server.Types.Types (Undefined (..))
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    Message,
    QUERY,
    SUBSCRIPTION,
    ScalarValue (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Event (..),
    Failure,
    PushEvents (..),
    Resolver,
    UnSubResolver,
    WithOperation,
    failure,
    lift,
    pushEvents,
    subscribe,
    unsafeInternalContext,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( HTTP,
    Input,
    Stream,
    WS,
  )
import Data.Text (pack)

class GQL (f :: * -> *) a where
  type Resolve (m :: * -> *) a :: *
  type ComposeRes (m :: * -> *) f a :: *

instance GQL f (a :: *) where
  type Resolve m a = m a
  type ComposeRes m f a = m (f a)

instance GQL f (a :: (* -> *) -> *) where
  type Resolve m a = m (a m)
  type ComposeRes m f a = m (f (a m))

-- Recursive Resolvers
type SmartResolver o e m a =
  (WithOperation o) =>
  Resolve (Resolver o e m) a

type ComposedResolver o e m f a =
  (WithOperation o) =>
  ComposeRes (Resolver o e m) f a

type ResolverQ e m a = SmartResolver QUERY e m a

type ResolverM e m a = SmartResolver MUTATION e m a

type ResolverS e m a = Resolver SUBSCRIPTION e m (a (Resolver QUERY e m))

-- newtype B m = B {field1 :: m Int}

-- resInt :: SmartResolver o () IO Int
-- resInt = pure 1

-- resB :: SmartResolver o () IO B
-- resB = pure B {field1 = pure 1}

-- resList :: ComposedResolver o () IO [] Int
-- resList = pure [1]

-- resListB :: ComposedResolver o () IO [] B
-- resListB = pure [B {field1 = pure 1}]

type Res = Resolver QUERY

type MutRes = Resolver MUTATION

type SubRes = Resolver SUBSCRIPTION

type IORes e = Res e IO

type IOMutRes e = MutRes e IO

type IOSubRes e = SubRes e IO

type ResolveQ e m a = Res e m (a (Res e m))

type ResolveM e m a = MutRes e m (a (MutRes e m))

type ResolveS e m a = SubRes e m (a (Res e m))

-- Subsciption Object Resolver Fields
type SubField m a = (m (a (UnSubResolver m)))

publish :: Monad m => [e] -> Resolver MUTATION e m ()
publish = pushEvents

-- resolves constant value on any argument
constRes :: (WithOperation o, Monad m) => b -> a -> Resolver o e m b
constRes = const . pure

constMutRes :: Monad m => [e] -> a -> args -> ResolverM e m a
constMutRes events value = const $ do
  publish events
  pure value

failRes :: (WithOperation o, Monad m) => String -> Resolver o e m a
failRes = failure . pack

liftEither :: (MonadTrans t, Monad (t m), Failure Message (t m)) => Monad m => m (Either String a) -> t m a
liftEither x = lift x >>= either (failure . pack) pure

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you can use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> *) (mut :: (* -> *) -> *) (sub :: (* -> *) -> *) = GQLRootResolver
  { queryResolver :: query (Resolver QUERY event m),
    mutationResolver :: mut (Resolver MUTATION event m),
    subscriptionResolver :: sub (Resolver SUBSCRIPTION event m)
  }
