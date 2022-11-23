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
import Data.Morpheus.Server.Deriving.Kinded.NamedResolverFun
  ( deriveNamedResolverFun,
  )
import Data.Morpheus.Server.Deriving.Utils.GRep (GRep)
import Data.Morpheus.Server.Deriving.Utils.GScan (ScanRef (..))
import Data.Morpheus.Server.Deriving.Utils.Gmap (Gmap)
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

decodeValues :: forall gql val m a. DecodeValuesConstraint val m a => UseDeriving gql val -> Proxy a -> [ValidValue] -> m [Maybe a]
decodeValues ctx _ xs = traverse decodeArg xs >>= resolveBatched
  where
    decodeArg :: ValidValue -> m (Dependency a)
    decodeArg = liftState . useDecodeValue (dirArgs ctx)

type DecodeValuesConstraint val m a =
  ( ResolveNamed m a,
    val (Dependency a),
    MonadResolver m
  )

class KindedNamedResolver namedRes resFun gql val (m :: Type -> Type) (k :: DerivingKind) a where
  kindedNamedResolver :: UseNamedResolver namedRes resFun gql val -> f k a -> [NamedResolver m]
  kindedNamedRefs :: UseNamedResolver namedRes resFun gql val -> f k a -> [ScanRef (namedRes m)]

instance
  ( DecodeValuesConstraint gql m a,
    EncodeScalar a,
    val (Dependency a),
    gql a,
    namedRes m a
  ) =>
  KindedNamedResolver namedRes resFun gql val m SCALAR a
  where
  kindedNamedResolver ctx _ =
    [ NamedResolver
        { resolverName = useTypename (dirGQL $ namedDrv ctx) (outputType proxy),
          resolverFun = decodeValues (namedDrv ctx) proxy >=> pure . map (maybe NamedNullResolver (NamedScalarResolver . encodeScalar))
        }
    ]
    where
      proxy = Proxy @a
  kindedNamedRefs ctx _ = [ScanType fp proxy]
    where
      fp = useFingerprint (dirGQL $ namedDrv ctx) (outputType proxy)
      proxy = Proxy @a

instance
  ( Generic a,
    gql a,
    gql [Maybe a],
    val (Dependency a),
    GRep gql (resFun m) (m (ResolverValue m)) (Rep a),
    Gmap (namedRes m) (Rep a),
    namedRes m a,
    DecodeValuesConstraint gql m a
  ) =>
  KindedNamedResolver namedRes resFun gql val m TYPE (a :: Type)
  where
  kindedNamedResolver ctx _ =
    [ NamedResolver
        { resolverName = useTypename (dirGQL $ namedDrv ctx) (outputType proxy),
          resolverFun = decodeValues (namedDrv ctx) proxy >=> deriveNamedResolverFun ctx
        }
    ]
    where
      proxy = Proxy @a

  kindedNamedRefs ctx _ = [ScanObject (useFingerprint (dirGQL $ namedDrv ctx) (outputType proxy)) proxy]
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
