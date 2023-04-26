{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Kinded.Type
  ( DeriveKindedType (..),
    DERIVE_TYPE,
    deriveScalarDefinition,
    deriveTypeGuardUnions,
  )
where

import Data.Morpheus.Server.Deriving.Internal.Schema.Directive (deriveDirectiveDefinition)
import Data.Morpheus.Server.Deriving.Internal.Schema.Type
  ( deriveScalarDefinition,
    deriveTypeDefinition,
    deriveTypeGuardUnions,
  )
import Data.Morpheus.Server.Deriving.Utils.GRep
  ( GRep,
  )
import Data.Morpheus.Server.Deriving.Utils.GScan (ScanRef (..))
import Data.Morpheus.Server.Deriving.Utils.Gmap (Gmap)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    catMap,
    unliftKind,
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( GQLNode (..),
    SchemaBuilder,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( FieldRep,
    UseDeriving (..),
    UseGQLType (..),
  )
import Data.Morpheus.Server.Types.Directives (GQLDirective)
import Data.Morpheus.Server.Types.Kind
  ( DIRECTIVE,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    scalarValidator,
  )
import GHC.Generics (Generic (Rep))
import Relude

type DERIVE_TYPE gql a =
  ( gql a,
    GRep gql gql (SchemaBuilder FieldRep) (Rep a)
  )

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType ctx (k :: DerivingKind) a where
  deriveKindedType ::
    ctx ~ UseDeriving gql v =>
    ctx ->
    CatType cat (f k a) ->
    SchemaBuilder (GQLNode cat)
  exploreKindedRefs :: ctx ~ UseDeriving gql v => ctx -> CatType cat (f k a) -> [ScanRef gql]

instance (gql a, ctx ~ UseDeriving gql v) => DeriveKindedType ctx WRAPPER (f a) where
  deriveKindedType UseDeriving {..} = useDeriveNode drvGQL . catMap (Proxy @a)
  exploreKindedRefs UseDeriving {..} = useExploreRef drvGQL . catMap (Proxy @a)

instance (DecodeScalar a, gql a, ctx ~ UseDeriving gql v) => DeriveKindedType ctx SCALAR a where
  deriveKindedType drv = fmap GQLTypeNode . deriveScalarDefinition scalarValidator drv . unliftKind
  exploreKindedRefs UseDeriving {..} proxy = [ScanLeaf (useFingerprint drvGQL p) p]
    where
      p = catMap (Proxy @a) proxy

instance (DERIVE_TYPE gql a, Gmap gql (Rep a), ctx ~ UseDeriving gql v) => DeriveKindedType ctx TYPE a where
  deriveKindedType drv = fmap GQLTypeNode . deriveTypeDefinition drv . unliftKind
  exploreKindedRefs UseDeriving {..} proxy = [ScanNode (useFingerprint drvGQL p) p]
    where
      p = catMap (Proxy @a) proxy

instance (ctx ~ UseDeriving gql v, GQLDirective a, gql a, v a, Gmap gql (Rep a)) => DeriveKindedType ctx DIRECTIVE a where
  deriveKindedType drv _ = GQLDirectiveNode <$> deriveDirectiveDefinition drv (Proxy @a)
  exploreKindedRefs UseDeriving {..} proxy = [ScanNode (useFingerprint drvGQL p) p]
    where
      p = catMap (Proxy @a) proxy