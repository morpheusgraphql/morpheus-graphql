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

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), (>>=))
import Data.Functor (($>), (<$>), Functor (..))
import Data.Map (Map)
import Data.Maybe (Maybe (..))
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
    INTERFACE,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( KindedType (..),
    TyContentM,
    UpdateDef (..),
    asObjectType,
    builder,
    fromSchema,
    unpackMs,
    updateByContent,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( TypeConstraint (..),
    TypeRep (..),
    toRep,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeData (..),
    __typeData,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    toSchema,
    withInput,
  )
import Data.Morpheus.Server.Types.Types
  ( Pair,
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
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, Rep)
import Language.Haskell.TH (Exp, Q)
import Prelude
  ( ($),
    (.),
    Bool (..),
  )

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
  deriveContent :: f a -> SchemaT kind (Maybe (FieldContent TRUE kind CONST))

instance (GQLType a, DeriveKindedType cat (KIND a) a) => DeriveType cat a where
  deriveType _ = deriveKindedType (KindedProxy :: KindedProxy (KIND a) a)
  deriveContent _ = deriveKindedContent (KindedProxy :: KindedProxy (KIND a) a)

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType (cat :: TypeCategory) (kind :: DerivingKind) a where
  deriveKindedType :: kinded kind a -> SchemaT cat ()
  deriveKindedContent :: kinded kind a -> SchemaT cat (Maybe (FieldContent TRUE cat CONST))
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

instance DeriveTypeConstraint OUT a => DeriveKindedType OUT INTERFACE a where
  deriveKindedType = updateByContent deriveInterfaceContent . setKind (Proxy @OUT)

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
  unpackMs (toRep (fieldContentConstraint kindedProxy) kindedProxy)
    >>= fmap (updateDef kindedProxy) . builder kindedProxy
