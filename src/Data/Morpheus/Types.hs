{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Data.Morpheus.Types
  ( GQLType (KIND, description, implements, getDescriptions, typeOptions, getDirectives),
    EncodeScalar (..),
    EncodeWrapper (..),
    DecodeScalar (..),
    DecodeWrapper (..),
    GQLRequest (..),
    GQLResponse (..),
    ID (..),
    ScalarValue (..),
    RootResolver (..),
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
    ResolverContext (..),
    -- Resolvers
    ResolverO,
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
    interface,
    SubscriptionField,
    App,
    RenderGQL,
    render,
    GQLTypeOptions (..),
  )
where

import Control.Applicative (pure)
import Control.Monad (Monad ((>>=)))
import Control.Monad.Fail (fail)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Either
  ( Either (..),
    either,
  )
import Data.Morpheus.Core
  ( App,
    RenderGQL,
    render,
  )
import Data.Morpheus.Server.Deriving.Schema
  ( DeriveType,
    SchemaT,
    deriveImplementsInterface,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLTypeOptions (..),
  )
import Data.Morpheus.Server.Types.Types (Undefined (..))
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.GQLWrapper
  ( DecodeWrapper (..),
    EncodeWrapper (..),
  )
import Data.Morpheus.Types.ID (ID (..))
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    Message,
    OUT,
    QUERY,
    SUBSCRIPTION,
    ScalarValue (..),
    TypeName,
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Failure,
    PushEvents (..),
    Resolver,
    ResolverContext (..),
    SubscriptionField,
    WithOperation,
    failure,
    pushEvents,
    subscribe,
    unsafeInternalContext,
  )
import Data.Proxy
  ( Proxy (..),
  )
import Prelude
  ( ($),
    (.),
    IO,
    String,
    const,
  )

class FlexibleResolver (f :: * -> *) (a :: k) where
  type Flexible (m :: * -> *) a :: *
  type Composed (m :: * -> *) f a :: *

instance FlexibleResolver f (a :: *) where
  type Flexible m a = m a
  type Composed m f a = m (f a)

instance FlexibleResolver f (a :: (* -> *) -> *) where
  type Flexible m a = m (a m)
  type Composed m f a = m (f (a m))

-- Recursive Resolvers
type ResolverO o e m a =
  (WithOperation o) =>
  Flexible (Resolver o e m) a

type ComposedResolver o e m f a =
  (WithOperation o) =>
  Composed (Resolver o e m) f a

type ResolverQ e m a = Flexible (Resolver QUERY e m) a

type ResolverM e m a = Flexible (Resolver MUTATION e m) a

type ResolverS e m a = Flexible (Resolver SUBSCRIPTION e m) a

{-# DEPRECATED Res "use ResolverQ" #-}

type Res = Resolver QUERY

{-# DEPRECATED MutRes "use ResolverM" #-}

type MutRes = Resolver MUTATION

{-# DEPRECATED SubRes "use ResolverS" #-}

type SubRes = Resolver SUBSCRIPTION

{-# DEPRECATED IORes "use ResolverQ" #-}

type IORes e = Res e IO

{-# DEPRECATED IOMutRes "use ResolverM" #-}

type IOMutRes e = MutRes e IO

{-# DEPRECATED IOSubRes "use ResolverS" #-}

type IOSubRes e = SubRes e IO

{-# DEPRECATED ResolveQ "use ResolverQ" #-}

type ResolveQ e m a = ResolverQ e m a

{-# DEPRECATED ResolveM "use ResolverM" #-}

type ResolveM e m a = ResolverM e m a

{-# DEPRECATED ResolveS "use ResolverS" #-}

type ResolveS e m a = ResolverS e m a

publish :: Monad m => [e] -> Resolver MUTATION e m ()
publish = pushEvents

-- resolves constant value on any argument
constRes :: (WithOperation o, Monad m) => b -> a -> Resolver o e m b
constRes = const . pure

constMutRes :: Monad m => [e] -> a -> args -> ResolverM e m a
constMutRes events value = const $ do
  publish events
  pure value

{-# DEPRECATED failRes "use \"fail\" from \"MonadFail\"" #-}
failRes ::
  ( Monad m,
    WithOperation o
  ) =>
  String ->
  Resolver o e m a
failRes = fail

liftEither :: (MonadTrans t, Monad (t m), Failure Message (t m)) => Monad m => m (Either String a) -> t m a
liftEither x = lift x >>= either (failure . msg) pure

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you can use __()__ for it.
data RootResolver (m :: * -> *) event (query :: (* -> *) -> *) (mut :: (* -> *) -> *) (sub :: (* -> *) -> *) = RootResolver
  { queryResolver :: query (Resolver QUERY event m),
    mutationResolver :: mut (Resolver MUTATION event m),
    subscriptionResolver :: sub (Resolver SUBSCRIPTION event m)
  }

interface :: (GQLType a, DeriveType OUT a) => Proxy a -> SchemaT OUT TypeName
interface = deriveImplementsInterface
