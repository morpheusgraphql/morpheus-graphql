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
    deriveInterfaceDefinition,
    deriveScalarDefinition,
    deriveTypeGuardUnions,
  )
where

import Data.Morpheus.Server.Deriving.Internal.Schema.Type
  ( deriveInterfaceDefinition,
    deriveScalarDefinition,
    deriveTypeDefinition,
    deriveTypeGuardUnions,
  )
import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveWith,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType,
    catMap,
    unliftKind,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
  )
import Data.Morpheus.Server.Types.Kind
  ( DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
  )
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    scalarValidator,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    TypeCategory,
    TypeDefinition (..),
  )
import GHC.Generics
import Relude

type DERIVE_TYPE gql c a = (gql a, DeriveWith gql gql (SchemaT c (Maybe (ArgumentsDefinition CONST))) (Rep a))

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType gql val (cat :: TypeCategory) (kind :: DerivingKind) a where
  deriveKindedType :: UseDeriving gql val -> CatType cat (f kind a) -> SchemaT cat (TypeDefinition cat CONST)

instance (gql a) => DeriveKindedType gql val cat WRAPPER (f a) where
  deriveKindedType UseDeriving {..} = useDeriveType dirGQL . catMap (Proxy @a)

instance (DecodeScalar a, gql a) => DeriveKindedType gql val cat SCALAR a where
  deriveKindedType drv = deriveScalarDefinition scalarValidator drv . unliftKind

instance DERIVE_TYPE gql cat a => DeriveKindedType gql val cat TYPE a where
  deriveKindedType drv = deriveTypeDefinition drv . unliftKind
