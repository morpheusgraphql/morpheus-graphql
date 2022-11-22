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

module Data.Morpheus.Server.Deriving.Kinded.NamedResolver
  ( KindedNamedResolver (..),
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( LiftOperation,
    NamedResolver (..),
    NamedResolverResult (..),
    Resolver,
    ResolverValue,
    liftResolverState,
  )
import Data.Morpheus.Server.Deriving.Kinded.NamedResolverFun
  ( deriveNamedResolverFun,
  )
import Data.Morpheus.Server.Deriving.Utils.DeriveGType (DeriveWith)
import Data.Morpheus.Server.Deriving.Utils.GFunctor
import Data.Morpheus.Server.Deriving.Utils.GTraversable (GmapProxy (..))
import Data.Morpheus.Server.Deriving.Utils.Kinded (outputType)
import Data.Morpheus.Server.Deriving.Utils.Use (UseDeriving (..), UseGQLType (useFingerprint, useTypename), UseNamedResolver (..), UseValue (useDecodeValue))
import Data.Morpheus.Server.NamedResolvers (Dependency, NamedResolverT (..), ResolveNamed (..))
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
import GHC.Generics (Rep)
import Relude

decodeValues :: forall gql val o e m a. DecodeValuesConstraint val o e m a => UseDeriving gql val -> Proxy a -> [ValidValue] -> Resolver o e m [Maybe a]
decodeValues ctx _ xs = traverse decodeArg xs >>= resolveBatched
  where
    decodeArg :: ValidValue -> Resolver o e m (Dependency a)
    decodeArg = liftResolverState . useDecodeValue (dirArgs ctx)

type DecodeValuesConstraint val o e m a =
  ( LiftOperation o,
    ResolveNamed (Resolver o e m) a,
    Monad m,
    val (Dependency a)
  )

class KindedNamedResolver namedRes resFun gql val (m :: Type -> Type) (k :: DerivingKind) a where
  kindedNamedResolver :: UseNamedResolver namedRes resFun gql val -> f k a -> [NamedResolver m]
  kindedNamedRefs :: UseNamedResolver namedRes resFun gql val -> f k a -> Maybe (GmapProxy (namedRes m))

instance
  ( DecodeValuesConstraint gql o e m a,
    EncodeScalar a,
    val (Dependency a),
    gql a
  ) =>
  KindedNamedResolver namedRes resFun gql val (Resolver o e m) SCALAR a
  where
  kindedNamedResolver ctx _ =
    [ NamedResolver
        { resolverName = useTypename (dirGQL $ namedDrv ctx) (outputType proxy),
          resolverFun = decodeValues (namedDrv ctx) proxy >=> pure . map (maybe NamedNullResolver (NamedScalarResolver . encodeScalar))
        }
    ]
    where
      proxy = Proxy @a
  kindedNamedRefs _ _ = Nothing

instance
  ( DecodeValuesConstraint gql o e m a,
    Generic a,
    gql a,
    gql [Maybe a],
    val (Dependency a),
    DeriveWith gql (resFun (Resolver o e m)) (Resolver o e m (ResolverValue (Resolver o e m))) (Rep a),
    GFunctor (namedRes (Resolver o e m)) (Rep a),
    namedRes (Resolver o e m) a
  ) =>
  KindedNamedResolver namedRes resFun gql val (Resolver o e m) TYPE (a :: Type)
  where
  kindedNamedResolver ctx _ =
    [ NamedResolver
        { resolverName = useTypename (dirGQL $ namedDrv ctx) (outputType proxy),
          resolverFun = decodeValues (namedDrv ctx) proxy >=> deriveNamedResolverFun ctx
        }
    ]
    where
      proxy = Proxy @a

  kindedNamedRefs ctx _ = Just $ GmapProxy (useFingerprint (dirGQL $ namedDrv ctx) (outputType proxy)) proxy
    where
      proxy = Proxy @a

instance namedRes m a => KindedNamedResolver namedRes resFun gql val m CUSTOM (NamedResolverT m a) where
  kindedNamedResolver ctx _ = useDeriveNamedResolvers ctx (Proxy @a)
  kindedNamedRefs ctx _ = useDeriveNamedRefs ctx (Proxy @a)

instance namedRes m a => KindedNamedResolver namedRes resFun gql val m CUSTOM (input -> a) where
  kindedNamedResolver ctx _ = useDeriveNamedResolvers ctx (Proxy @a)
  kindedNamedRefs ctx _ = useDeriveNamedRefs ctx (Proxy @a)

instance namedRes m a => KindedNamedResolver namedRes resFun gql val m WRAPPER (f a) where
  kindedNamedResolver ctx _ = useDeriveNamedResolvers ctx (Proxy @a)
  kindedNamedRefs ctx _ = useDeriveNamedRefs ctx (Proxy @a)
