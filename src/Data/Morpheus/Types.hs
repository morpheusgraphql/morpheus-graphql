{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | GQL Types
module Data.Morpheus.Types
  ( Event (..),
    -- Type Classes
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
    Res,
    MutRes,
    SubRes,
    IORes,
    IOMutRes,
    IOSubRes,
    Resolver,
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    lift,
    liftEither,
    ResolveQ,
    ResolveM,
    ResolveS,
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

type Res = Resolver QUERY

type MutRes = Resolver MUTATION

type SubRes = Resolver SUBSCRIPTION

type IORes e = Res e IO

type IOMutRes e = MutRes e IO

type IOSubRes e = SubRes e IO

-- Recursive Resolvers
type ResolveO o e m a = Resolve (Resolver o e m) a

type ResolveQ e m a = ResolveO QUERY e m a

type ResolveM e m a = ResolveO MUTATION e m a

type ResolveS e m a = SubRes e m (a (Res e m))

type ResolveList o e m a = ComposeRes (Resolver o e m) [] a

resList :: ResolveList QUERY () IO Int
resList = pure [1]

newtype B m = B (m Int)

resB :: ResolveList QUERY () IO B
resB = pure [B $ pure 2]

-- Subsciption Object Resolver Fields
type SubField m a = (m (a (UnSubResolver m)))

publish :: Monad m => [e] -> Resolver MUTATION e m ()
publish = pushEvents

-- resolves constant value on any argument
constRes :: (WithOperation o, Monad m) => b -> a -> Resolver o e m b
constRes = const . pure

constMutRes :: Monad m => [e] -> a -> args -> MutRes e m a
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
