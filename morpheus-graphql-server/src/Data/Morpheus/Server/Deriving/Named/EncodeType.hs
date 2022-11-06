{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Named.EncodeType
  ( deriveResolver,
    EncodeTypeConstraint,
    DeriveNamedResolver (..),
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( LiftOperation,
    NamedResolver (..),
    NamedResolverResult (..),
    Resolver,
    liftResolverState,
  )
import Data.Morpheus.Server.Deriving.Decode
  ( Decode,
    decode,
  )
import Data.Morpheus.Server.Deriving.Named.EncodeValue
  ( EncodeFieldKind,
    FieldConstraint,
    encodeResolverValue,
    getTypeName,
  )
import Data.Morpheus.Server.Deriving.Utils.GTraversable
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (KindedProxy))
import Data.Morpheus.Server.NamedResolvers (Dependency, NamedResolverT (..), ResolveNamed (..))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType,
    KIND,
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Types.GQLScalar (EncodeScalar (..))
import Data.Morpheus.Types.Internal.AST
  ( ValidValue,
  )
import Relude

deriveResolver :: Mappable (DeriveNamedResolver m) [NamedResolver m] KindedProxy
deriveResolver = Mappable deriveNamedResolver

type EncodeTypeConstraint m a =
  ( GFmap
      (ScanConstraint (DeriveNamedResolver m))
      (KIND (a (NamedResolverT m)))
      (a (NamedResolverT m)),
    DeriveNamedResolver
      m
      (KIND (a (NamedResolverT m)))
      (a (NamedResolverT m)),
    GQLType (a (NamedResolverT m))
  )

class DeriveNamedResolver (m :: Type -> Type) (k :: DerivingKind) a where
  deriveNamedResolver :: f k a -> [NamedResolver m]

instance
  ( GQLType a,
    DecodeValuesConstraint o e m a,
    EncodeScalar a
  ) =>
  DeriveNamedResolver (Resolver o e m) SCALAR a
  where
  deriveNamedResolver _ =
    [ NamedResolver
        { resolverName = getTypeName proxy,
          resolverFun = decodeValues proxy >=> pure . map (maybe NamedNullResolver (NamedScalarResolver . encodeScalar))
        }
    ]
    where
      proxy = Proxy @a

type DecodeValuesConstraint o e m a =
  ( LiftOperation o,
    ResolveNamed (Resolver o e m) a,
    Monad m,
    Decode (Dependency a)
  )

decodeValues :: forall o e m a. DecodeValuesConstraint o e m a => Proxy a -> [ValidValue] -> Resolver o e m [Maybe a]
decodeValues _ xs = traverse decodeArg xs >>= resolveBatched
  where
    decodeArg :: ValidValue -> Resolver o e m (Dependency a)
    decodeArg = liftResolverState . decode

instance
  ( GQLType a,
    DecodeValuesConstraint o e m a,
    EncodeFieldKind (KIND a) (Resolver o e m) a,
    FieldConstraint (Resolver o e m) a
  ) =>
  DeriveNamedResolver (Resolver o e m) TYPE (a :: Type)
  where
  deriveNamedResolver _ =
    [ NamedResolver
        { resolverName = getTypeName proxy,
          resolverFun = decodeValues proxy >=> encodeResolverValue
        }
    ]
    where
      proxy = Proxy @a

instance DeriveNamedResolver m (KIND a) a => DeriveNamedResolver m CUSTOM (NamedResolverT m a) where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

instance DeriveNamedResolver m (KIND a) a => DeriveNamedResolver m CUSTOM (input -> a) where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

instance DeriveNamedResolver m (KIND a) a => DeriveNamedResolver m WRAPPER (f a) where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)
