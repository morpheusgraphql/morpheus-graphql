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
    ComposedSubField,
    Input,
    Stream,
    WS,
    HTTP,
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

type ResolverS e m a = Resolver SUBSCRIPTION e m (a (Resolver QUERY e m))

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

-- Subsciption Object Resolver Fields
type SubField m a = (m (a (UnSubResolver m)))

type ComposedSubField m f a = (m (f (a (UnSubResolver m))))

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
liftEither x = lift x >>= either (failure . pack) pure

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you can use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> *) (mut :: (* -> *) -> *) (sub :: (* -> *) -> *) = GQLRootResolver
  { queryResolver :: query (Resolver QUERY event m),
    mutationResolver :: mut (Resolver MUTATION event m),
    subscriptionResolver :: sub (Resolver SUBSCRIPTION event m)
  }
