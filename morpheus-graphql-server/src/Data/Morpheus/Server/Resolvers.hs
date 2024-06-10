{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Resolvers
  ( ResolveNamed (..),
    NamedResolverT (..),
    resolve,
    NamedResolvers (..),
    RootResolver (..),
    defaultRootResolver,
    ResolverO,
    ComposedResolver,
    constRes,
    ResolverQ,
    ResolverM,
    ResolverS,
    useBatched,
    ignoreBatching,
    Flexible,
    Composed,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( MonadResolver (..),
    Resolver,
  )
import Data.Morpheus.Server.Types.NamedResolvers
  ( NamedResolverT (..),
    ResolveNamed (..),
    ignoreBatching,
    resolve,
    useBatched,
  )
import Data.Morpheus.Server.Types.Types
  ( Undefined (..),
  )
import Data.Morpheus.Types.Internal.AST
import Relude hiding (Undefined)

data
  NamedResolvers
    (m :: Type -> Type)
    event
    (qu :: (Type -> Type) -> Type)
    (mu :: (Type -> Type) -> Type)
    (su :: (Type -> Type) -> Type)
  = ( ResolveNamed
        (Resolver QUERY event m)
        (qu (NamedResolverT (Resolver QUERY event m)))
    ) =>
    NamedResolvers

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you can use __()__ for it.
data
  RootResolver
    (m :: Type -> Type)
    event
    (query :: (Type -> Type) -> Type)
    (mutation :: (Type -> Type) -> Type)
    (subscription :: (Type -> Type) -> Type)
  = RootResolver
  { queryResolver :: query (Resolver QUERY event m),
    mutationResolver :: mutation (Resolver MUTATION event m),
    subscriptionResolver :: subscription (Resolver SUBSCRIPTION event m)
  }

defaultRootResolver :: RootResolver m event Undefined Undefined Undefined
defaultRootResolver =
  RootResolver
    { queryResolver = Undefined False,
      mutationResolver = Undefined False,
      subscriptionResolver = Undefined False
    }

class FlexibleResolver (f :: Type -> Type) (a :: k) where
  type Flexible (m :: Type -> Type) a :: Type
  type Composed (m :: Type -> Type) f a :: Type

instance FlexibleResolver f (a :: Type) where
  type Flexible m a = m a
  type Composed m f a = m (f a)

instance FlexibleResolver f (a :: (Type -> Type) -> Type) where
  type Flexible m a = m (a m)
  type Composed m f a = m (f (a m))

type ComposedResolver o e m f a = Composed (Resolver o e m) f a

type ResolverO o e m a = Flexible (Resolver o e m) a

type ResolverQ e m a = ResolverO QUERY e m a

type ResolverM e m a = ResolverO MUTATION e m a

type ResolverS e m a = ResolverO SUBSCRIPTION e m a

constRes :: (MonadResolver m) => b -> a -> m b
constRes = const . pure
