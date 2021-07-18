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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema
  ( compileTimeSchemaValidation,
    DeriveType,
    deriveImplementsInterface,
    deriveSchema,
    SchemaConstraints,
    SchemaT,
  )
where

-- MORPHEUS

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    resultOr,
  )
import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Deriving.Schema.Enum
  ( buildEnumTypeContent,
  )
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( KindedType (..),
    TyContent,
    TyContentM,
    fromSchema,
    updateByContent,
  )
import Data.Morpheus.Server.Deriving.Schema.Object
  ( asObjectType,
    buildObjectTypeContent,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Schema.Union (buildUnionTypeContent)
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    TypeConstraint (..),
    TypeRep (..),
    isEmptyConstraint,
    toRep,
    unpackMonad,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeData (..),
    __typeData,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    extendImplements,
    toSchema,
    withInput,
  )
import Data.Morpheus.Server.Types.Types
  ( Pair,
    TypeGuard,
  )
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    scalarValidator,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    CONST,
    FieldContent (..),
    FieldContent (..),
    FieldsDefinition,
    GQLErrors,
    IN,
    LEAF,
    MUTATION,
    OBJECT,
    OUT,
    QUERY,
    SUBSCRIPTION,
    Schema (..),
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    UnionMember (memberName),
    ValidationError,
    fieldsToArguments,
  )
import Data.Morpheus.Utils.Kinded
  ( CategoryValue (..),
    KindedProxy (..),
    inputType,
    kinded,
    outputType,
    setKind,
  )
import GHC.Generics (Rep)
import Language.Haskell.TH (Exp, Q)
import Relude

type SchemaConstraints event (m :: * -> *) query mutation subscription =
  ( DeriveTypeConstraintOpt OUT (query (Resolver QUERY event m)),
    DeriveTypeConstraintOpt OUT (mutation (Resolver MUTATION event m)),
    DeriveTypeConstraintOpt OUT (subscription (Resolver SUBSCRIPTION event m))
  )

type DeriveTypeConstraintOpt kind a =
  ( Generic a,
    GQLType a,
    TypeRep (DeriveType kind) (TyContentM kind) (Rep a),
    TypeRep (DeriveType kind) (SchemaT kind ()) (Rep a)
  )

-- | normal morpheus server validates schema at runtime (after the schema derivation).
--   this method allows you to validate it at compile time.
compileTimeSchemaValidation ::
  (SchemaConstraints event m qu mu su) =>
  proxy (root m event qu mu su) ->
  Q Exp
compileTimeSchemaValidation =
  fromSchema
    . (deriveSchema >=> validateSchema True defaultConfig)

deriveSchema ::
  forall
    root
    proxy
    m
    e
    query
    mut
    subs
    f.
  ( SchemaConstraints e m query mut subs,
    Failure GQLErrors f
  ) =>
  proxy (root m e query mut subs) ->
  f (Schema CONST)
deriveSchema _ = resultOr failure pure schema
  where
    schema = toSchema schemaT
    schemaT ::
      SchemaT
        OUT
        ( TypeDefinition OBJECT CONST,
          TypeDefinition OBJECT CONST,
          TypeDefinition OBJECT CONST
        )
    schemaT =
      (,,)
        <$> deriveObjectType (Proxy @(query (Resolver QUERY e m)))
        <*> deriveObjectType (Proxy @(mut (Resolver MUTATION e m)))
        <*> deriveObjectType (Proxy @(subs (Resolver SUBSCRIPTION e m)))

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class DeriveType (kind :: TypeCategory) (a :: *) where
  deriveType :: f a -> SchemaT kind ()
  deriveContent :: f a -> TyContentM kind

instance (GQLType a, DeriveKindedType cat (KIND a) a) => DeriveType cat a where
  deriveType _ = deriveKindedType (KindedProxy :: KindedProxy (KIND a) a)
  deriveContent _ = deriveKindedContent (KindedProxy :: KindedProxy (KIND a) a)

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType (cat :: TypeCategory) (kind :: DerivingKind) a where
  deriveKindedType :: kinded kind a -> SchemaT cat ()
  deriveKindedContent :: kinded kind a -> TyContentM cat
  deriveKindedContent _ = pure Nothing

type DeriveTypeConstraint kind a =
  ( DeriveTypeConstraintOpt kind a,
    CategoryValue kind
  )

-- SCALAR
instance (GQLType a, DeriveType cat a) => DeriveKindedType cat WRAPPER (f a) where
  deriveKindedType _ = deriveType (KindedProxy :: KindedProxy cat a)

instance (GQLType a, DecodeScalar a) => DeriveKindedType cat SCALAR a where
  deriveKindedType = updateByContent deriveScalarContent . setKind (Proxy @LEAF)

instance DeriveTypeConstraint OUT a => DeriveKindedType OUT TYPE a where
  deriveKindedType = deriveOutputType

instance DeriveTypeConstraint IN a => DeriveKindedType IN TYPE a where
  deriveKindedType = deriveInputType

instance DeriveType cat a => DeriveKindedType cat CUSTOM (Resolver o e m a) where
  deriveKindedType _ = deriveType (Proxy @a)

-- Tuple
instance DeriveType cat (Pair k v) => DeriveKindedType cat CUSTOM (k, v) where
  deriveKindedType _ = deriveType (Proxy @(Pair k v))

-- Map
instance DeriveType cat [Pair k v] => DeriveKindedType cat CUSTOM (Map k v) where
  deriveKindedType _ = deriveType (Proxy @[Pair k v])

instance
  ( DeriveTypeConstraint OUT interface,
    DeriveTypeConstraint OUT union
  ) =>
  DeriveKindedType OUT CUSTOM (TypeGuard interface union)
  where
  deriveKindedType _ = do
    updateByContent deriveInterfaceContent interfaceProxy
    content <- deriveTypeContent (OutputType :: KindedType OUT union)
    unionNames <- getUnionNames content
    extendImplements interfaceName unionNames
    where
      interfaceName :: TypeName
      interfaceName = gqlTypeName (__typeData interfaceProxy)
      interfaceProxy :: KindedProxy OUT interface
      interfaceProxy = KindedProxy
      unionProxy :: KindedProxy OUT union
      unionProxy = KindedProxy
      getUnionNames :: TypeContent TRUE OUT CONST -> SchemaT OUT [TypeName]
      getUnionNames DataUnion {unionMembers} = pure $ toList $ memberName <$> unionMembers
      getUnionNames DataObject {} = pure [gqlTypeName (__typeData unionProxy)]
      getUnionNames _ = failure ["guarded type must be an union or object" :: ValidationError]

instance
  ( GQLType b,
    DeriveType OUT b,
    DeriveTypeConstraint IN a
  ) =>
  DeriveKindedType OUT CUSTOM (a -> m b)
  where
  deriveKindedContent _ = Just . FieldArgs <$> deriveArgumentDefinition (Proxy @a)
  deriveKindedType _ = deriveType (outputType $ Proxy @b)

deriveScalarContent :: (DecodeScalar a) => f k a -> SchemaT cat (TypeContent TRUE LEAF CONST)
deriveScalarContent = pure . DataScalar . scalarValidator

deriveInterfaceContent :: DeriveTypeConstraint OUT a => f a -> SchemaT OUT (TypeContent TRUE OUT CONST)
deriveInterfaceContent = fmap DataInterface . deriveFields . outputType

deriveArgumentDefinition :: DeriveTypeConstraint IN a => f a -> SchemaT OUT (ArgumentsDefinition CONST)
deriveArgumentDefinition = withInput . fmap fieldsToArguments . deriveFields . inputType

deriveFields :: DeriveTypeConstraint kind a => KindedType kind a -> SchemaT kind (FieldsDefinition kind CONST)
deriveFields kindedType = deriveTypeContent kindedType >>= withObject kindedType

deriveInputType :: DeriveTypeConstraint IN a => f a -> SchemaT IN ()
deriveInputType = updateByContent deriveTypeContent . inputType

deriveOutputType :: DeriveTypeConstraint OUT a => f a -> SchemaT OUT ()
deriveOutputType = updateByContent deriveTypeContent . outputType

deriveObjectType :: DeriveTypeConstraint OUT a => f a -> SchemaT OUT (TypeDefinition OBJECT CONST)
deriveObjectType = asObjectType (deriveFields . outputType)

deriveImplementsInterface :: (GQLType a, DeriveType OUT a) => f a -> SchemaT OUT TypeName
deriveImplementsInterface x = deriveType (outputType x) $> gqlTypeName (__typeData (kinded (Proxy @OUT) x))

fieldContentConstraint :: f kind a -> TypeConstraint (DeriveType kind) (TyContentM kind) Proxy
fieldContentConstraint _ = TypeConstraint deriveFieldContent

deriveFieldContent :: forall f kind a. (DeriveType kind a) => f a -> TyContentM kind
deriveFieldContent _ = deriveType kindedProxy *> deriveContent kindedProxy
  where
    kindedProxy :: KindedProxy kind a
    kindedProxy = KindedProxy

deriveTypeContent ::
  forall kind a.
  DeriveTypeConstraint kind a =>
  KindedType kind a ->
  SchemaT kind (TypeContent TRUE kind CONST)
deriveTypeContent kindedProxy =
  unpackMonad
    (toRep (fieldContentConstraint kindedProxy) kindedProxy)
    >>= buildTypeContent kindedProxy

buildTypeContent ::
  (GQLType a, CategoryValue kind) =>
  KindedType kind a ->
  [ConsRep (TyContent kind)] ->
  SchemaT kind (TypeContent TRUE kind CONST)
buildTypeContent scope [ConsRep {consFields}] = buildObjectTypeContent scope consFields
buildTypeContent scope cons | all isEmptyConstraint cons = buildEnumTypeContent scope (consName <$> cons)
buildTypeContent scope cons = buildUnionTypeContent scope cons
