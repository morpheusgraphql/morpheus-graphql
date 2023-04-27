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
    scanNode,
  )
where

import Data.Morpheus.Generic
  ( Gmap,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive (deriveDirectiveDefinition)
import Data.Morpheus.Server.Deriving.Internal.Schema.Type
  ( DERIVE_TYPE,
    deriveScalarDefinition,
    deriveTypeDefinition,
    deriveTypeGuardUnions,
  )
import Data.Morpheus.Server.Deriving.Utils.GScan (ScanRef (..))
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    catMap,
    inputType,
    unliftKind,
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( SchemaBuilder,
    liftResult,
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
  deriveKindedType :: ctx ~ UseDeriving gql v => ctx -> CatType cat (f k a) -> SchemaBuilder (GQLTypeNode cat)
  exploreKindedRefs :: ctx ~ UseDeriving gql v => ctx -> CatType cat (f k a) -> [ScanRef gql]

instance (gql a, ctx ~ UseDeriving gql v) => DeriveKindedType ctx WRAPPER (f a) where
  deriveKindedType UseDeriving {..} = useDeriveNode drvGQL . catMap (Proxy @a)
  exploreKindedRefs UseDeriving {..} = useExploreRef drvGQL . catMap (Proxy @a)

scanLeaf :: (c a, gql a) => UseGQLType gql -> CatType k a -> [ScanRef c]
scanLeaf gql p = [ScanLeaf (useFingerprint gql p) p]

scanNode :: (c a, gql a, Gmap c (Rep a)) => Bool -> UseGQLType gql -> CatType k a -> [ScanRef c]
scanNode visible gql p = [ScanNode visible (useFingerprint gql p) p]

instance (DecodeScalar a, gql a, ctx ~ UseDeriving gql v) => DeriveKindedType ctx SCALAR a where
  deriveKindedType drv = liftResult . deriveScalarDefinition scalarValidator drv . unliftKind
  exploreKindedRefs UseDeriving {..} proxy = scanLeaf drvGQL (catMap (Proxy @a) proxy)

instance (DERIVE_TYPE gql a, Gmap gql (Rep a), ctx ~ UseDeriving gql v) => DeriveKindedType ctx TYPE a where
  deriveKindedType drv = liftResult . fmap (uncurry GQLTypeNode) . deriveTypeDefinition drv . unliftKind
  exploreKindedRefs UseDeriving {..} proxy = scanNode True drvGQL (catMap (Proxy @a) proxy)

instance (DERIVE_TYPE gql a, Gmap gql (Rep a), ctx ~ UseDeriving gql v, GQLDirective a, v a) => DeriveKindedType ctx DIRECTIVE a where
  deriveKindedType drv _ = GQLDirectiveNode <$> (liftResult (deriveTypeDefinition drv proxy) >>= deriveDirectiveDefinition drv proxy . fst)
    where
      proxy = inputType (Proxy @a)
  exploreKindedRefs UseDeriving {..} proxy
    | excludeFromSchema (Proxy @a) = []
    | otherwise = scanNode True drvGQL (catMap (Proxy @a) proxy)