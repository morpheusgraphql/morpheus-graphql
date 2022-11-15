{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Kinded.Resolver
  ( KindedResolver (..),
  )
where

import Control.Monad.Except (MonadError)
import qualified Data.Map as M
import Data.Morpheus.App.Internal.Resolving
  ( LiftOperation,
    Resolver,
    ResolverValue (..),
    getArguments,
    liftResolverState,
  )
import Data.Morpheus.Server.Deriving.Internal.Resolve.Explore
import Data.Morpheus.Server.Deriving.Kinded.Value (KindedValue)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( ContextValue (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving,
    UseResolver (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLValue,
    KIND,
    decodeArguments,
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.Types
  ( TypeGuard (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
  )
import Data.Morpheus.Types.GQLWrapper (EncodeWrapper (..))
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
  )
import Relude

-- ENCODE GQL KIND
class KindedResolver gql res (kind :: DerivingKind) (m :: Type -> Type) (a :: Type) where
  kindedResolver :: (UseResolver res, UseDeriving gql val) -> ContextValue kind a -> m (ResolverValue m)

instance (EncodeWrapper f, Monad m, res m a) => KindedResolver gql res WRAPPER m (f a) where
  kindedResolver (res, _) = encodeWrapper (useEncodeResolver res) . unContextValue

instance (EncodeScalar a, Monad m) => KindedResolver gql res SCALAR m a where
  kindedResolver _ = pure . ResScalar . encodeScalar . unContextValue

instance (MonadError GQLError m, EXPLORE gql res m a) => KindedResolver gql res TYPE m a where
  kindedResolver ctx = pure . useExploreResolvers ctx . unContextValue

--  Map
instance (Monad m, res m [(k, v)]) => KindedResolver gql res CUSTOM m (Map k v) where
  kindedResolver (res, _) = useEncodeResolver res . M.toList . unContextValue

--  INTERFACE Types
instance (MonadError GQLError m, EXPLORE gql res m guard, EXPLORE gql res m union) => KindedResolver gql res CUSTOM m (TypeGuard guard union) where
  kindedResolver ctx (ContextValue (ResolveType value)) = pure (useExploreResolvers ctx value)
  kindedResolver ctx (ContextValue (ResolveInterface value)) = pure (useExploreResolvers ctx value)

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance
  ( Generic a,
    Monad m,
    res (Resolver o e m) b,
    LiftOperation o,
    KindedValue GQLType GQLValue (KIND a) a
  ) =>
  KindedResolver gql res CUSTOM (Resolver o e m) (a -> b)
  where
  kindedResolver (res, _) (ContextValue f) =
    getArguments
      >>= liftResolverState . decodeArguments
      >>= useEncodeResolver res . f

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (Monad m, res (Resolver o e m) b, LiftOperation o) => KindedResolver gql res CUSTOM (Resolver o e m) (Resolver o e m b) where
  kindedResolver (res, _) (ContextValue value) = value >>= useEncodeResolver res