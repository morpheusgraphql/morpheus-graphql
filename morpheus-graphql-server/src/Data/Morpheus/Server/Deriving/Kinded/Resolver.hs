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
import Data.Morpheus.Server.Deriving.Utils.AST
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( ContextValue (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (dirArgs),
    UseResolver (..),
    UseValue (useDecodeValue),
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
class KindedResolver gql res val (kind :: DerivingKind) (m :: Type -> Type) (a :: Type) where
  kindedResolver :: UseResolver res gql val -> ContextValue kind a -> m (ResolverValue m)

instance (EncodeWrapper f, Monad m, res m a) => KindedResolver gql res val WRAPPER m (f a) where
  kindedResolver res = encodeWrapper (useEncodeResolver res) . unContextValue

instance (EncodeScalar a, Monad m) => KindedResolver gql res val SCALAR m a where
  kindedResolver _ = pure . ResScalar . encodeScalar . unContextValue

instance (MonadError GQLError m, EXPLORE gql res m a) => KindedResolver gql res val TYPE m a where
  kindedResolver ctx = pure . useExploreResolvers ctx . unContextValue

--  Map
instance (Monad m, res m [(k, v)]) => KindedResolver gql res val CUSTOM m (Map k v) where
  kindedResolver res = useEncodeResolver res . M.toList . unContextValue

--  INTERFACE Types
instance (MonadError GQLError m, EXPLORE gql res m guard, EXPLORE gql res m union) => KindedResolver gql res val CUSTOM m (TypeGuard guard union) where
  kindedResolver ctx (ContextValue (ResolveType value)) = pure (useExploreResolvers ctx value)
  kindedResolver ctx (ContextValue (ResolveInterface value)) = pure (useExploreResolvers ctx value)

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance
  ( Generic a,
    Monad m,
    res (Resolver o e m) b,
    LiftOperation o,
    val a
  ) =>
  KindedResolver gql res val CUSTOM (Resolver o e m) (a -> b)
  where
  kindedResolver res (ContextValue f) =
    getArguments
      >>= liftResolverState . useDecodeValue (dirArgs $ resDrv res) . argumentsToObject
      >>= useEncodeResolver res . f

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (Monad m, res (Resolver o e m) b, LiftOperation o) => KindedResolver gql res val CUSTOM (Resolver o e m) (Resolver o e m b) where
  kindedResolver res (ContextValue value) = value >>= useEncodeResolver res
