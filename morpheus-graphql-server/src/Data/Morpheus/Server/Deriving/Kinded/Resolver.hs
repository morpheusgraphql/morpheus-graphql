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

import Data.Morpheus.App.Internal.Resolving
  ( MonadResolver (..),
    ResolverValue (..),
    getArguments,
  )
import Data.Morpheus.Internal.Utils (toAssoc)
import Data.Morpheus.Server.Deriving.Internal.Resolve.Explore
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( ContextValue (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Types
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseGQLValue (useDecodeValue),
    UseResolver (..),
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
import Relude

-- ENCODE GQL KIND
class KindedResolver ctx (k :: DerivingKind) (m :: Type -> Type) (a :: Type) where
  kindedResolver :: (MonadResolver m, UseResolver res gql val ~ ctx) => ctx -> ContextValue k a -> m (ResolverValue m)

instance (UseResolver res gql val ~ ctx, EncodeWrapper f, res m a) => KindedResolver ctx WRAPPER m (f a) where
  kindedResolver res = encodeWrapper (useEncodeResolver res) . unContextValue

instance (EncodeScalar a) => KindedResolver ctx SCALAR m a where
  kindedResolver _ = pure . ResScalar . encodeScalar . unContextValue

instance (UseResolver res gql val ~ ctx, EXPLORE gql res m a) => KindedResolver ctx TYPE m a where
  kindedResolver ctx = pure . useExploreResolvers ctx . unContextValue

--  Map
instance (UseResolver res gql val ~ ctx, res m [(k, v)], Ord k) => KindedResolver ctx CUSTOM m (Map k v) where
  kindedResolver res = useEncodeResolver res . toAssoc . unContextValue

--  INTERFACE Types
instance (UseResolver res gql val ~ ctx, EXPLORE gql res m guard, EXPLORE gql res m union) => KindedResolver ctx CUSTOM m (TypeGuard guard union) where
  kindedResolver ctx (ContextValue (ResolveType value)) = pure (useExploreResolvers ctx value)
  kindedResolver ctx (ContextValue (ResolveInterface value)) = pure (useExploreResolvers ctx value)

instance (UseResolver res gql val ~ ctx, Generic a, res m b, val a) => KindedResolver ctx CUSTOM m (a -> b) where
  kindedResolver res (ContextValue f) =
    getArguments
      >>= liftState . useDecodeValue res . argumentsToObject
      >>= useEncodeResolver res . f

instance (UseResolver res gql val ~ ctx, res m a) => KindedResolver ctx CUSTOM m (m a) where
  kindedResolver res (ContextValue value) = value >>= useEncodeResolver res
