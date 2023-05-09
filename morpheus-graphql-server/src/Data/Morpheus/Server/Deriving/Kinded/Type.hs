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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Kinded.Type
  ( DeriveKindedType (..),
    DERIVE_TYPE,
    deriveScalarDefinition,
    deriveTypeGuardUnions,
    scanNode,
  )
where

import Data.Morpheus.Generic
  ( Gmap,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Internal.Directive (deriveDirectiveDefinition)
import Data.Morpheus.Server.Deriving.Internal.Type
  ( DERIVE_TYPE,
    deriveScalarDefinition,
    deriveTypeDefinition,
    deriveTypeGuardUnions,
  )
import Data.Morpheus.Server.Deriving.Utils.GScan
  ( FreeCatType,
    ScanRef,
    freeLeaf,
    freeNode,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
    mapCat,
    unliftKind,
  )
import Data.Morpheus.Server.Deriving.Utils.Types (GQLTypeNode (..))
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
  )
import Data.Morpheus.Server.Types.Directives (GQLDirective (..))
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

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType ctx (k :: DerivingKind) a where
  deriveKindedType :: (ctx ~ UseDeriving gql v) => ctx -> CatType cat (f k a) -> GQLResult (GQLTypeNode cat)
  exploreKindedRefs :: (ctx ~ UseDeriving gql v) => ctx -> CatType cat (f k a) -> [ScanRef FreeCatType gql]

instance (gql a, ctx ~ UseDeriving gql v) => DeriveKindedType ctx WRAPPER (f a) where
  deriveKindedType ctx = useDeriveNode ctx . mapCat (Proxy @a)
  exploreKindedRefs ctx = useExploreRef ctx . mapCat (Proxy @a)

scanLeaf :: (c a, UseGQLType ctx gql, gql a) => ctx -> CatType k a -> [ScanRef FreeCatType c]
scanLeaf gql p = [freeLeaf (useFingerprint gql p) p]

scanNode :: (c a, gql a, UseGQLType ctx gql, Gmap c (Rep a)) => Bool -> ctx -> CatType k a -> [ScanRef FreeCatType c]
scanNode visible gql p = [freeNode visible (useFingerprint gql p) p]

instance (DecodeScalar a, gql a, ctx ~ UseDeriving gql v) => DeriveKindedType ctx SCALAR a where
  deriveKindedType ctx = deriveScalarDefinition scalarValidator ctx . unliftKind
  exploreKindedRefs ctx proxy = scanLeaf ctx (mapCat (Proxy @a) proxy)

instance (DERIVE_TYPE gql a, Gmap gql (Rep a), ctx ~ UseDeriving gql v) => DeriveKindedType ctx TYPE a where
  deriveKindedType ctx = fmap (uncurry GQLTypeNode) . deriveTypeDefinition ctx . unliftKind
  exploreKindedRefs ctx proxy = scanNode True ctx (mapCat (Proxy @a) proxy)

instance (DERIVE_TYPE gql a, Gmap gql (Rep a), ctx ~ UseDeriving gql v, GQLDirective a, v a) => DeriveKindedType ctx DIRECTIVE a where
  deriveKindedType drv _ = GQLDirectiveNode <$> (deriveTypeDefinition drv proxy >>= deriveDirectiveDefinition drv proxy . fst)
    where
      proxy = inputType (Proxy @a)
  exploreKindedRefs ctx proxy
    | excludeFromSchema (Proxy @a) = []
    | otherwise = scanNode True ctx (mapCat (Proxy @a) proxy)
