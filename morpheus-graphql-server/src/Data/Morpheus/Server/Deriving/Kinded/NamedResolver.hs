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
  ( MonadResolver (..),
    NamedResolver (..),
    NamedResolverResult (..),
    ResolverValue,
  )
import Data.Morpheus.Generic (GRep, Gmap)
import Data.Morpheus.Server.Deriving.Kinded.NamedResolverFun
  ( deriveNamedResolverFun,
  )
import Data.Morpheus.Server.Deriving.Utils.GScan (ScanRef (..))
import Data.Morpheus.Server.Deriving.Utils.Kinded (outputType)
import Data.Morpheus.Server.Deriving.Utils.Use (UseDeriving (..), UseGQLType (useFingerprint, useTypename), UseNamedResolver (..), UseValue (useDecodeValue))
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.NamedResolvers (Dependency, NamedResolverT (..), ResolveNamed (..))
import Data.Morpheus.Types.GQLScalar (EncodeScalar (..))
import Data.Morpheus.Types.Internal.AST
  ( ValidValue,
  )
import GHC.Generics (Rep)
import Relude

type DECODE_VALUES val m a = (ResolveNamed m a, val (Dependency a), MonadResolver m)

decodeValues :: DECODE_VALUES val m a => UseDeriving gql val -> Proxy a -> [ValidValue] -> m [Maybe a]
decodeValues ctx _ xs = traverse (liftState . useDecodeValue (drvArgs ctx)) xs >>= resolveBatched

class KindedNamedResolver ctx (k :: DerivingKind) (m :: Type -> Type) a where
  kindedNamedResolver :: UseNamedResolver namedRes resFun gql val ~ ctx => ctx -> f k a -> [NamedResolver m]
  kindedNamedRefs :: UseNamedResolver namedRes resFun gql val ~ ctx => ctx -> f k a -> [ScanRef (namedRes m)]

instance
  ( UseNamedResolver namedRes resFun gql val ~ ctx,
    DECODE_VALUES val m a,
    gql a,
    namedRes m a,
    EncodeScalar a
  ) =>
  KindedNamedResolver ctx SCALAR m a
  where
  kindedNamedResolver ctx _ =
    [ NamedResolver
        { resolverName = useTypename (drvGQL $ namedDrv ctx) (outputType proxy),
          resolverFun = decodeValues (namedDrv ctx) proxy >=> pure . map (maybe NamedNullResolver (NamedScalarResolver . encodeScalar))
        }
    ]
    where
      proxy = Proxy @a
  kindedNamedRefs ctx _ = [ScanLeaf fp (outputType proxy)]
    where
      fp = useFingerprint (drvGQL $ namedDrv ctx) (outputType proxy)
      proxy = Proxy @a

instance
  ( UseNamedResolver namedRes resFun gql val ~ ctx,
    DECODE_VALUES val m a,
    gql a,
    namedRes m a,
    Generic a,
    gql [Maybe a],
    GRep gql (resFun m) (m (ResolverValue m)) (Rep a),
    Gmap (namedRes m) (Rep a)
  ) =>
  KindedNamedResolver ctx TYPE m (a :: Type)
  where
  kindedNamedResolver ctx _ =
    [ NamedResolver
        { resolverName = useTypename (drvGQL $ namedDrv ctx) (outputType proxy),
          resolverFun = decodeValues (namedDrv ctx) proxy >=> deriveNamedResolverFun ctx
        }
    ]
    where
      proxy = Proxy @a

  kindedNamedRefs ctx _ = [ScanNode True (useFingerprint (drvGQL $ namedDrv ctx) (outputType proxy)) (outputType proxy)]
    where
      proxy = Proxy @a

instance (UseNamedResolver namedRes resFun gql val ~ ctx, namedRes m a) => KindedNamedResolver ctx CUSTOM m (NamedResolverT m a) where
  kindedNamedResolver ctx _ = useDeriveNamedResolvers ctx (Proxy @a)
  kindedNamedRefs ctx _ = useDeriveNamedRefs ctx (Proxy @a)

instance (UseNamedResolver namedRes resFun gql val ~ ctx, namedRes m a) => KindedNamedResolver ctx CUSTOM m (input -> a) where
  kindedNamedResolver ctx _ = useDeriveNamedResolvers ctx (Proxy @a)
  kindedNamedRefs ctx _ = useDeriveNamedRefs ctx (Proxy @a)

instance (UseNamedResolver namedRes resFun gql val ~ ctx, namedRes m a) => KindedNamedResolver ctx WRAPPER m (f a) where
  kindedNamedResolver ctx _ = useDeriveNamedResolvers ctx (Proxy @a)
  kindedNamedRefs ctx _ = useDeriveNamedRefs ctx (Proxy @a)
