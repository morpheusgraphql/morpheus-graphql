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
import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Kind
  ( ENUM,
    GQL_KIND,
    INTERFACE,
    SCALAR,
    TYPE,
  )
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( KindedProxy (..),
    KindedType (..),
    TyContentM,
    UpdateDef (..),
    asObjectType,
    builder,
    fromSchema,
    inputType,
    outputType,
    setProxyType,
    unpackMs,
    updateByContent,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( TypeConstraint (..),
    TypeRep (..),
    genericTo,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeData (..),
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    toSchema,
  )
import Data.Morpheus.Server.Types.Types
  ( MapKind,
    Pair,
  )
import Data.Morpheus.Types.GQLScalar (GQLScalar (..))
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
import Data.Morpheus.Types.Internal.Resolving
  ( Resolver,
    SubscriptionField (..),
    resultOr,
  )
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import GHC.Generics (Generic, Rep)
import Language.Haskell.TH (Exp, Q)
import Prelude
  ( ($),
    (.),
    Bool (..),
  )

type SchemaConstraints event (m :: * -> *) query mutation subscription =
  ( DeriveTypeConstraint OUT (query (Resolver QUERY event m)),
    DeriveTypeConstraint OUT (mutation (Resolver MUTATION event m)),
    DeriveTypeConstraint OUT (subscription (Resolver SUBSCRIPTION event m))
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
        ( TypeDefinition OBJECT CONST,
          TypeDefinition OBJECT CONST,
          TypeDefinition OBJECT CONST
        )
    schemaT =
      (,,)
        <$> deriveObjectType (Proxy @(query (Resolver QUERY e m)))
        <*> deriveObjectType (Proxy @(mut (Resolver MUTATION e m)))
        <*> deriveObjectType (Proxy @(subs (Resolver SUBSCRIPTION e m)))

instance {-# OVERLAPPABLE #-} (GQLType a, DeriveKindedType cat (KIND a) a) => DeriveType cat a where
  deriveType _ = deriveKindedType (Proxy @cat) (KindedProxy :: KindedProxy (KIND a) a)

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class DeriveType (kind :: TypeCategory) (a :: *) where
  deriveType :: f kind a -> SchemaT ()

  deriveContent :: f kind a -> SchemaT (Maybe (FieldContent TRUE kind CONST))
  deriveContent _ = pure Nothing

deriveTypeWith :: DeriveType cat a => f a -> kinded cat b -> SchemaT ()
deriveTypeWith x = deriveType . setProxyType x

-- Maybe
instance DeriveType cat a => DeriveType cat (Maybe a) where
  deriveType = deriveTypeWith (Proxy @a)

-- List
instance DeriveType cat a => DeriveType cat [a] where
  deriveType = deriveTypeWith (Proxy @a)

-- Tuple
instance DeriveType cat (Pair k v) => DeriveType cat (k, v) where
  deriveType = deriveTypeWith (Proxy @(Pair k v))

-- Set
instance DeriveType cat [a] => DeriveType cat (Set a) where
  deriveType = deriveTypeWith (Proxy @[a])

-- Map
instance DeriveType cat (MapKind k v Maybe) => DeriveType cat (Map k v) where
  deriveType = deriveTypeWith (Proxy @(MapKind k v Maybe))

-- Resolver : a -> Resolver b
instance
  ( GQLType b,
    DeriveType OUT b,
    DeriveTypeConstraint IN a
  ) =>
  DeriveType OUT (a -> m b)
  where
  deriveContent _ = Just . FieldArgs <$> deriveArgumentDefinition (Proxy @a)
  deriveType _ = deriveType (outputType $ Proxy @b)

instance (DeriveType OUT a) => DeriveType OUT (SubscriptionField a) where
  deriveType _ = deriveType (KindedProxy :: KindedProxy OUT a)

--  GQL Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (DeriveType cat b) => DeriveType cat (Resolver fo e m b) where
  deriveType = deriveTypeWith (Proxy @b)

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType (cat :: TypeCategory) (kind :: GQL_KIND) a where
  deriveKindedType :: f cat -> proxy kind a -> SchemaT ()

type DeriveTypeConstraint kind a =
  ( Generic a,
    GQLType a,
    TypeRep (DeriveType kind) (TyContentM kind) (Rep a),
    TypeRep (DeriveType kind) (SchemaT ()) (Rep a)
  )

-- SCALAR
instance (GQLType a, GQLScalar a) => DeriveKindedType cat SCALAR a where
  deriveKindedType _ = updateByContent deriveScalarContent

-- ENUM
instance DeriveTypeConstraint IN a => DeriveKindedType cat ENUM a where
  deriveKindedType _ = deriveInputType

instance DeriveTypeConstraint OUT a => DeriveKindedType cat INTERFACE a where
  deriveKindedType _ = updateByContent deriveInterfaceContent

instance DeriveTypeConstraint OUT a => DeriveKindedType OUT TYPE a where
  deriveKindedType _ = deriveOutputType

instance DeriveTypeConstraint IN a => DeriveKindedType IN TYPE a where
  deriveKindedType _ = deriveInputType

deriveScalarContent :: (GQLScalar a) => f a -> SchemaT (TypeContent TRUE LEAF CONST)
deriveScalarContent = pure . DataScalar . scalarValidator

deriveInterfaceContent :: DeriveTypeConstraint OUT a => f a -> SchemaT (TypeContent TRUE OUT CONST)
deriveInterfaceContent = fmap DataInterface . deriveFields . outputType

deriveArgumentDefinition :: DeriveTypeConstraint IN a => f a -> SchemaT (ArgumentsDefinition CONST)
deriveArgumentDefinition = fmap fieldsToArguments . deriveFields . inputType

deriveFields :: DeriveTypeConstraint kind a => KindedType kind a -> SchemaT (FieldsDefinition kind CONST)
deriveFields kindedType = deriveTypeContent kindedType >>= withObject kindedType

deriveInputType :: DeriveTypeConstraint IN a => f a -> SchemaT ()
deriveInputType = updateByContent deriveTypeContent . inputType

deriveOutputType :: DeriveTypeConstraint OUT a => f a -> SchemaT ()
deriveOutputType = updateByContent deriveTypeContent . outputType

deriveObjectType :: DeriveTypeConstraint OUT a => f a -> SchemaT (TypeDefinition OBJECT CONST)
deriveObjectType = asObjectType (deriveFields . outputType)

deriveImplementsInterface :: (GQLType a, DeriveType OUT a) => f a -> SchemaT TypeName
deriveImplementsInterface x = deriveType (outputType x) $> gqlTypeName (__type x)

fieldContentConstraint :: f kind a -> TypeConstraint (DeriveType kind) (TyContentM kind) Proxy
fieldContentConstraint _ = TypeConstraint deriveFieldContent

deriveFieldContent :: forall f kind a. (DeriveType kind a) => f a -> TyContentM kind
deriveFieldContent _ = deriveType kinded *> deriveContent kinded
  where
    kinded :: KindedProxy kind a
    kinded = KindedProxy

deriveTypeContent ::
  DeriveTypeConstraint kind a =>
  KindedType kind a ->
  SchemaT (TypeContent TRUE kind CONST)
deriveTypeContent kinded =
  unpackMs (genericTo (fieldContentConstraint kinded) kinded)
    >>= fmap (updateDef kinded) . builder kinded
