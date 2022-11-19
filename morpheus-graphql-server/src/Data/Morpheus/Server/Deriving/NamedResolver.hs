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

module Data.Morpheus.Server.Deriving.NamedResolver
  ( EncodeTypeConstraint,
    KindedNamedResolver (..),
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
import Data.Morpheus.Server.Deriving.Utils.GTraversable
  ( GFmap,
    ScanConstraint,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (KindedProxy), outputType)
import Data.Morpheus.Server.Deriving.Utils.Use (UseDeriving (..), UseGQLType (useTypename), UseNamedResolver (..), UseValue (useDecodeValue))
import Data.Morpheus.Server.NamedResolvers (Dependency, NamedResolverT (..), ResolveNamed (..))
import Data.Morpheus.Server.Types.GQLType
  ( KIND,
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

type EncodeTypeConstraint res gql val m a =
  ( GFmap (ScanConstraint (KindedNamedResolver res gql val m)) (KIND (a (NamedResolverT m))) (a (NamedResolverT m)),
    KindedNamedResolver res gql val m (KIND (a (NamedResolverT m))) (a (NamedResolverT m)),
    gql (a (NamedResolverT m))
  )

class KindedNamedResolver res gql val (m :: Type -> Type) (k :: DerivingKind) a where
  kindedNamedResolver :: UseNamedResolver res gql val -> f k a -> [NamedResolver m]

instance
  ( DecodeValuesConstraint gql o e m a,
    EncodeScalar a,
    val (Dependency a),
    gql a
  ) =>
  KindedNamedResolver res gql val (Resolver o e m) SCALAR a
  where
  kindedNamedResolver ctx _ =
    [ NamedResolver
        { resolverName = useTypename (dirGQL $ namedDrv ctx) (outputType proxy),
          resolverFun = decodeValues (namedDrv ctx) proxy >=> pure . map (maybe NamedNullResolver (NamedScalarResolver . encodeScalar))
        }
    ]
    where
      proxy = Proxy @a

instance
  ( DecodeValuesConstraint gql o e m a,
    Generic a,
    gql a,
    gql [Maybe a],
    val (Dependency a),
    DeriveWith gql (res (Resolver o e m)) (Resolver o e m (ResolverValue (Resolver o e m))) (Rep a)
  ) =>
  KindedNamedResolver res gql val (Resolver o e m) TYPE (a :: Type)
  where
  kindedNamedResolver ctx _ =
    [ NamedResolver
        { resolverName = useTypename (dirGQL $ namedDrv ctx) (outputType proxy),
          resolverFun = decodeValues (namedDrv ctx) proxy >=> deriveNamedResolverFun ctx
        }
    ]
    where
      proxy = Proxy @a

instance KindedNamedResolver res gql val m (KIND a) a => KindedNamedResolver res gql val m CUSTOM (NamedResolverT m a) where
  kindedNamedResolver ctx _ = kindedNamedResolver ctx (KindedProxy :: KindedProxy (KIND a) a)

instance KindedNamedResolver res gql val m (KIND a) a => KindedNamedResolver res gql val m CUSTOM (input -> a) where
  kindedNamedResolver ctx _ = kindedNamedResolver ctx (KindedProxy :: KindedProxy (KIND a) a)

instance KindedNamedResolver res gql val m (KIND a) a => KindedNamedResolver res gql val m WRAPPER (f a) where
  kindedNamedResolver ctx _ = kindedNamedResolver ctx (KindedProxy :: KindedProxy (KIND a) a)
