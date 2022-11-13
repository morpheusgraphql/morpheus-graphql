{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema.DeriveKinded
  ( DeriveKindedType (..),
    DeriveArgs (..),
    DERIVE_TYPE,
    DeriveFieldArguments (..),
    HasArguments,
  )
where

import Data.Morpheus.Internal.Ext ((<:>))
import Data.Morpheus.Internal.Utils (singleton)
import Data.Morpheus.Server.Deriving.Schema.Internal (CatType)
import Data.Morpheus.Server.Deriving.Schema.TypeContent
  ( deriveFields,
    deriveScalarDefinition,
    deriveTypeDefinition,
    injectType,
  )
import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveWith,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    catMap,
    inputType,
    unliftKind,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy (symbolName)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseArguments (..),
    UseDirective (..),
    UseGQLType (..),
  )
import Data.Morpheus.Server.Types.Internal (TypeData (..))
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
  )
import Data.Morpheus.Server.Types.Types
  ( Arg (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    scalarValidator,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    FALSE,
    IN,
    OUT,
    TRUE,
    TypeCategory,
    TypeDefinition (..),
    TypeRef (..),
    fieldsToArguments,
    mkField,
  )
import GHC.Generics
import GHC.TypeLits
import Relude

type DERIVE_TYPE gql c a = (gql a, DeriveWith gql gql (SchemaT c (Maybe (ArgumentsDefinition CONST))) (Rep a))

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType gql dir (cat :: TypeCategory) (kind :: DerivingKind) a where
  deriveKindedType :: UseDirective gql dir -> CatType cat (f kind a) -> SchemaT cat (TypeDefinition cat CONST)

instance (gql a) => DeriveKindedType gql dir cat WRAPPER (f a) where
  deriveKindedType UseDirective {..} = useDeriveType dirGQL . catMap (Proxy @a)

instance (DecodeScalar a, gql a) => DeriveKindedType gql dir cat SCALAR a where
  deriveKindedType dir = deriveScalarDefinition scalarValidator dir . unliftKind

instance DERIVE_TYPE gql cat a => DeriveKindedType gql dir cat TYPE a where
  deriveKindedType dir = deriveTypeDefinition dir . unliftKind

type family HasArguments a where
  HasArguments (a -> b) = TRUE
  HasArguments b = FALSE

class DeriveFieldArguments gql dir (k :: Bool) a where
  deriveFieldArguments :: UseDirective gql dir -> f k a -> SchemaT OUT (Maybe (ArgumentsDefinition CONST))

instance DeriveFieldArguments gql dir FALSE a where
  deriveFieldArguments _ _ = pure Nothing

instance (gql b, dir a) => DeriveFieldArguments gql dir TRUE (a -> b) where
  deriveFieldArguments UseDirective {..} _ = do
    a <- useDeriveArguments dirArgs (Proxy @a)
    b <- useDeriveFieldArguments dirGQL (OutputType :: CatType OUT b)
    case b of
      Just x -> Just <$> (a <:> x)
      Nothing -> pure $ Just a

class DeriveArgs gql (k :: DerivingKind) a where
  deriveArgs :: UseDirective gql dir -> f k a -> SchemaT IN (ArgumentsDefinition CONST)

instance (DERIVE_TYPE gql IN a) => DeriveArgs gql TYPE a where
  deriveArgs dir = fmap fieldsToArguments . deriveFields dir . inputType

instance (KnownSymbol name, gql a) => DeriveArgs gql CUSTOM (Arg name a) where
  deriveArgs dir@UseDirective {..} _ = do
    injectType dir proxy
    pure $ fieldsToArguments $ singleton argName $ mkField Nothing argName argTypeRef
    where
      proxy = InputType :: CatType IN a
      argName = symbolName (Proxy @name)
      argTypeRef = TypeRef {typeConName = gqlTypeName, typeWrappers = gqlWrappers}
      TypeData {gqlTypeName, gqlWrappers} = useTypeData dirGQL proxy
