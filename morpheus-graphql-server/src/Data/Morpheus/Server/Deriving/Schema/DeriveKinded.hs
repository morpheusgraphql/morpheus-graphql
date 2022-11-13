{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
  )
import Data.Morpheus.Internal.Ext ((<:>))
import Data.Morpheus.Internal.Utils (singleton)
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( CatType,
    TyContentM,
  )
import Data.Morpheus.Server.Deriving.Schema.TypeContent
  ( deriveFields,
    deriveInterfaceDefinition,
    deriveScalarDefinition,
    deriveTypeDefinition,
    deriveTypeGuardUnions,
    injectType,
  )
import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveWith,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    catMap,
    inputType,
    outputType,
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
    extendImplements,
  )
import Data.Morpheus.Server.Types.Types
  ( Arg (..),
    TypeGuard,
  )
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    scalarValidator,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    FieldContent (..),
    IN,
    OUT,
    ScalarDefinition (..),
    TypeCategory,
    TypeDefinition (..),
    TypeRef (..),
    Value,
    fieldsToArguments,
    mkField,
  )
import GHC.Generics
import GHC.TypeLits
import Relude

type DERIVE_TYPE gql k a = (gql a, DeriveWith gql gql (TyContentM k) (Rep a))

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType gql dir (cat :: TypeCategory) (kind :: DerivingKind) a where
  deriveKindedContent :: UseDirective gql dir -> CatType cat (f kind a) -> TyContentM cat
  deriveKindedContent _ _ = pure Nothing

  deriveTypeV :: UseDirective gql dir -> CatType cat (f kind a) -> SchemaT cat (TypeDefinition cat CONST)

instance (gql a) => DeriveKindedType gql dir cat WRAPPER (f a) where
  deriveTypeV UseDirective {..} = useDeriveType dirGQL . catMap (Proxy @a)

instance (DecodeScalar a, gql a) => DeriveKindedType gql dir cat SCALAR a where
  deriveTypeV dir = deriveScalarDefinition scalarValidator dir . unliftKind

instance DERIVE_TYPE gql cat a => DeriveKindedType gql dir cat TYPE a where
  deriveTypeV dir = deriveTypeDefinition dir . unliftKind

instance (gql a) => DeriveKindedType gql dir cat CUSTOM (Resolver o e m a) where
  deriveTypeV UseDirective {..} = useDeriveType dirGQL . catMap (Proxy @a)

instance (gql (Value CONST)) => DeriveKindedType gql dir cat CUSTOM (Value CONST) where
  deriveTypeV dir = deriveScalarDefinition (const $ ScalarDefinition pure) dir . unliftKind

instance (gql [(k, v)]) => DeriveKindedType gql dir cat CUSTOM (Map k v) where
  deriveTypeV UseDirective {..} = useDeriveType dirGQL . catMap (Proxy @[(k, v)])

instance
  ( DERIVE_TYPE gql OUT interface,
    DERIVE_TYPE gql OUT union
  ) =>
  DeriveKindedType gql dir OUT CUSTOM (TypeGuard interface union)
  where
  deriveTypeV dir OutputType = do
    unionNames <- deriveTypeGuardUnions dir union
    extendImplements (useTypename (dirGQL dir) interface) unionNames
    deriveInterfaceDefinition dir interface
    where
      interface = OutputType :: CatType OUT interface
      union = OutputType :: CatType OUT union

instance (gql b, dir a) => DeriveKindedType gql dir OUT CUSTOM (a -> b) where
  deriveKindedContent UseDirective {..} OutputType = do
    a <- useDeriveArguments dirArgs (Proxy @a)
    b <- useDeriveContent dirGQL (OutputType :: CatType OUT b)
    case b of
      Just (FieldArgs x) -> Just . FieldArgs <$> (a <:> x)
      Nothing -> pure $ Just (FieldArgs a)
  deriveTypeV UseDirective {..} OutputType = useDeriveType dirGQL (outputType $ Proxy @b)

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
