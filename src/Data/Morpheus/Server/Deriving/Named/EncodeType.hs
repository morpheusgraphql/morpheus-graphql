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
    Resolver,
    ResolverState,
    ResolverValue,
    liftResolverState,
  )
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.NamedResolvers (NamedResolverT (..), ResolveNamed (Dep, resolveNamed))
import Data.Morpheus.Server.Deriving.Decode
  ( Decode (decode),
  )
import Data.Morpheus.Server.Deriving.Named.EncodeValue
  ( Encode,
    EncodeFieldKind,
    encodeResolverValue,
    getTypeName,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( TypeRep (..),
  )
import Data.Morpheus.Server.Deriving.Utils.GTraversable
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (KindedProxy))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType,
    KIND,
  )
import Data.Morpheus.Types.Internal.AST
  ( ValidValue,
  )
import GHC.Generics
  ( Generic (..),
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

class DeriveNamedResolver (m :: * -> *) (k :: DerivingKind) a where
  deriveNamedResolver :: f k a -> [NamedResolver m]

instance DeriveNamedResolver m SCALAR a where
  deriveNamedResolver _ = []

instance
  ( Monad m,
    LiftOperation o,
    Generic a,
    GQLType a,
    EncodeFieldKind (KIND a) (Resolver o e m) a,
    Decode (Dep a),
    ResolveNamed (Resolver o e m) a,
    TypeRep (Encode (Resolver o e m)) (Resolver o e m (ResolverValue (Resolver o e m))) (Rep a)
  ) =>
  DeriveNamedResolver (Resolver o e m) TYPE (a :: *)
  where
  deriveNamedResolver _ =
    [ NamedResolver
        { resolverName = getTypeName (Proxy @a),
          resolver = resolve >=> encodeResolverValue
        }
    ]
    where
      resolve :: ValidValue -> Resolver o e m a
      resolve x = liftResolverState (decode x :: ResolverState (Dep a)) >>= resolveNamed

instance DeriveNamedResolver m (KIND a) a => DeriveNamedResolver m CUSTOM (NamedResolverT m a) where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

instance DeriveNamedResolver m (KIND a) a => DeriveNamedResolver m CUSTOM (input -> a) where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

instance DeriveNamedResolver m (KIND a) a => DeriveNamedResolver m WRAPPER (f a) where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)
