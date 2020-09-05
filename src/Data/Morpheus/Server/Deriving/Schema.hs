{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema
  ( compileTimeSchemaValidation,
    DeriveType,
    deriveOutType,
    deriveSchema,
    SchemaConstraints,
    SchemaT,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), (>>=), sequence_)
import Control.Monad.Fail (fail)
import Data.Functor ((<$>), Functor (..))
import Data.Map (Map)
import Data.Maybe (Maybe (..))
import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Kind
  ( ENUM,
    GQL_KIND,
    INPUT,
    INTERFACE,
    OUTPUT,
    SCALAR,
  )
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( KindedProxy (..),
    KindedType (..),
    UpdateDef (..),
    buildType,
    builder,
    inputType,
    mkObjectType,
    outputType,
    setProxyType,
    unpackMs,
    updateLib,
    withObject,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( TypeConstraint (..),
    TypeRep (..),
    genericTo,
    repToValues,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    closeWith,
    setMutation,
    setSubscription,
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
    VALID,
    fieldsToArguments,
    initTypeLib,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Resolver,
    Result (..),
    SubscriptionField (..),
  )
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import GHC.Generics (Generic, Rep)
import Language.Haskell.TH (Exp, Q)
import Prelude
  ( ($),
    (.),
    Bool (..),
    Show (..),
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

fromSchema :: Eventless (Schema VALID) -> Q Exp
fromSchema Success {} = [|()|]
fromSchema Failure {errors} = fail (show errors)

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
deriveSchema _ = case schema of
  Success {result} -> pure result
  Failure {errors} -> failure errors
  where
    schema = closeWith (initTypeLib <$> query <* mutation <* subscription)
    query = deriveObjectType (Proxy @(query (Resolver QUERY e m)))
    mutation = deriveObjectType (Proxy @(mut (Resolver MUTATION e m))) >>= setMutation
    ------------------------------
    subscription =
      deriveObjectType (Proxy @(subs (Resolver SUBSCRIPTION e m)))
        >>= setSubscription

instance {-# OVERLAPPABLE #-} (GQLType a, DeriveKindedType (KIND a) a) => DeriveType cat a where
  deriveType _ = deriveKindedType (KindedProxy :: KindedProxy (KIND a) a)

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
class DeriveKindedType (kind :: GQL_KIND) a where
  deriveKindedType :: proxy kind a -> SchemaT ()

-- SCALAR
instance (GQLType a, GQLScalar a) => DeriveKindedType SCALAR a where
  deriveKindedType _ = updateLib scalarType (Proxy @a)
    where
      scalarType :: Proxy a -> SchemaT (TypeDefinition LEAF CONST)
      scalarType p = pure $ buildType p (DataScalar $ scalarValidator p)

-- ENUM
instance DeriveTypeConstraint IN a => DeriveKindedType ENUM a where
  deriveKindedType _ = derivingData $ inputType (Proxy @a)

instance DeriveTypeConstraint IN a => DeriveKindedType INPUT a where
  deriveKindedType _ = derivingData $ inputType (Proxy @a)

instance DeriveTypeConstraint OUT a => DeriveKindedType OUTPUT a where
  deriveKindedType _ = derivingData $ outputType (Proxy @a)

type DeriveTypeConstraint kind a =
  ( GQL_TYPE a,
    TypeRep (DeriveType kind) (TyContentM kind) (Rep a),
    TypeRep (DeriveType kind) (SchemaT ()) (Rep a)
  )

instance DeriveTypeConstraint OUT a => DeriveKindedType INTERFACE a where
  deriveKindedType _ = updateLib deriveInterfaceType (Proxy @a)

deriveInterfaceType :: DeriveTypeConstraint OUT a => f (a :: *) -> SchemaT (TypeDefinition OUT CONST)
deriveInterfaceType proxy = buildType proxy . DataInterface <$> deriveObjectFields proxy

derivingData ::
  forall kind a.
  DeriveTypeConstraint kind a =>
  KindedType kind a ->
  SchemaT ()
derivingData kindedType =
  updateLib bla (Proxy @a)
  where
    bla proxy = buildType proxy <$> deriveTypeContent kindedType

type GQL_TYPE a = (Generic a, GQLType a)

deriveArgumentDefinition :: DeriveTypeConstraint IN a => f a -> SchemaT (ArgumentsDefinition CONST)
deriveArgumentDefinition = fmap fieldsToArguments . deriveFields . inputType

deriveObjectFields ::
  DeriveTypeConstraint OUT a => f a -> SchemaT (FieldsDefinition OUT CONST)
deriveObjectFields = deriveFields . outputType

deriveFields ::
  DeriveTypeConstraint kind a =>
  KindedType kind a ->
  SchemaT (FieldsDefinition kind CONST)
deriveFields kindedType = deriveTypeContent kindedType >>= withObject kindedType

deriveOutType :: forall a. (GQLType a, DeriveType OUT a) => Proxy a -> SchemaT ()
deriveOutType _ = deriveType (KindedProxy :: KindedProxy OUT a)

deriveObjectType ::
  DeriveTypeConstraint OUT a =>
  proxy a ->
  SchemaT (TypeDefinition OBJECT CONST)
deriveObjectType proxy = (`mkObjectType` __typeName proxy) <$> deriveObjectFields proxy

deriveFieldValue :: forall f kind a. (DeriveType kind a) => f a -> SchemaT (Maybe (FieldContent TRUE kind CONST))
deriveFieldValue _ = deriveContent (KindedProxy :: KindedProxy k a)

deriveFieldTypes ::
  forall kind a.
  (GQLType a, TypeRep (DeriveType kind) (SchemaT ()) (Rep a), Generic a) =>
  KindedType kind a ->
  SchemaT ()
deriveFieldTypes kinded =
  sequence_
    $ repToValues
    $ genericTo
      (TypeConstraint (`deriveTypeWith` kinded) :: TypeConstraint (DeriveType kind) (SchemaT ()) Proxy)
      (Proxy @a)

type TyContentM kind = (SchemaT (Maybe (FieldContent TRUE kind CONST)))

-- Object Fields
deriveTypeContent ::
  forall kind a.
  DeriveTypeConstraint kind a =>
  KindedType kind a ->
  SchemaT (TypeContent TRUE kind CONST)
deriveTypeContent scope =
  deriveFieldTypes scope
    *> unpackMs
      ( genericTo
          (TypeConstraint deriveFieldValue :: TypeConstraint (DeriveType kind) (TyContentM kind) Proxy)
          proxy
      )
    >>= fmap (updateDef proxy) . builder scope
  where
    proxy = Proxy @a
