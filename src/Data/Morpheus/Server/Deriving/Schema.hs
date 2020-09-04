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
import Control.Monad ((>=>), (>>=), Monad)
import Control.Monad.Fail (fail)
import Data.Foldable (concatMap, traverse_)
import Data.Functor (($>), (<$>), Functor (..))
import Data.List (partition)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    Namespace (..),
    empty,
    singleton,
  )
import Data.Morpheus.Kind
  ( ENUM,
    GQL_KIND,
    INPUT,
    INTERFACE,
    OUTPUT,
    SCALAR,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    FieldRep (..),
    ResRep (..),
    TypeConstraint (..),
    TypeRep (..),
    fieldTypeName,
    genericTo,
    isEmptyConstraint,
    isUnionRef,
    repToValues,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    closeWith,
    concatSchemaT,
    updateExperimental,
    updateSchema,
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
    DataEnumValue (..),
    DataFingerprint (..),
    DataUnion,
    Description,
    Directives,
    ELEM,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldName (..),
    FieldsDefinition,
    GQLErrors,
    IN,
    LEAF,
    MUTATION,
    Message,
    OBJECT,
    OUT,
    QUERY,
    SUBSCRIPTION,
    Schema (..),
    TRUE,
    Token,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    UnionMember (..),
    VALID,
    ValidationErrors,
    fieldsToArguments,
    initTypeLib,
    isNotSystemTypeName,
    isTypeDefined,
    mkEnumContent,
    mkField,
    mkInputValue,
    mkType,
    mkUnionMember,
    msg,
    safeDefineType,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Resolver,
    Result (..),
    SubscriptionField (..),
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Traversable (traverse)
import GHC.Generics (Generic, Rep)
import Language.Haskell.TH (Exp, Q)
import Prelude
  ( ($),
    (.),
    Bool (..),
    Show (..),
    const,
    map,
    null,
    otherwise,
    sequence,
  )

type SchemaConstraints event (m :: * -> *) query mutation subscription =
  ( DeriveTypeConstraint OUT (query (Resolver QUERY event m)),
    DeriveTypeConstraint OUT (mutation (Resolver MUTATION event m)),
    DeriveTypeConstraint OUT (subscription (Resolver SUBSCRIPTION event m))
  )

-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data KindedProxy k a
  = KindedProxy

data KindedType (cat :: TypeCategory) a where
  InputType :: KindedType IN a
  OutputType :: KindedType OUT a

inputType :: f a -> KindedType IN a
inputType _ = InputType

outputType :: f a -> KindedType OUT a
outputType _ = OutputType

deriving instance Show (KindedType cat a)

setProxyType :: f b -> kinded k a -> KindedProxy k b
setProxyType _ _ = KindedProxy

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
deriveSchema _ = case querySchema >>= mutationSchema >>= subscriptionSchema of
  Success {result} -> pure result
  Failure {errors} -> failure errors
  where
    querySchema =
      closeWith $
        initTypeLib
          <$> deriveObjectType (Proxy @(query (Resolver QUERY e m)))
    ------------------------------
    mutationSchema lib = closeWith $ do
      mutation <- deriveObjectType $ Proxy @(mut (Resolver MUTATION e m))
      pure $ lib {mutation = optionalType mutation}
    ------------------------------
    subscriptionSchema lib = closeWith $ do
      subscription <- deriveObjectType $ Proxy @(subs (Resolver SUBSCRIPTION e m))
      pure (lib {subscription = optionalType subscription})

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
  deriveType _ = do
    deriveType $ outputType $ Proxy @b
    deriveFieldTypes $ inputType $ Proxy @a

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
  deriveKindedType _ = updateLib scalarType (pure ()) (Proxy @a)
    where
      scalarType :: Proxy a -> TypeDefinition LEAF CONST
      scalarType = buildType $ DataScalar $ scalarValidator (Proxy @a)

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
  deriveKindedType _ = updateExperimental (interface $ Proxy @a)
    where
      interface proxy = do
        deriveFieldTypes $ outputType proxy
        (`buildType` proxy) <$> deriveInterfaceContent proxy

deriveInterfaceContent :: DeriveTypeConstraint OUT a => f (a :: *) -> SchemaT (TypeContent TRUE OUT CONST)
deriveInterfaceContent x = fmap DataInterface (deriveObjectFields x)

derivingData ::
  forall kind a.
  DeriveTypeConstraint kind a =>
  KindedType kind a ->
  SchemaT ()
derivingData kindedType = updateExperimental $ do
  deriveFieldTypes kindedType
  (`buildType` Proxy @a) <$> deriveTypeContent kindedType

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

withObject :: (GQLType a) => KindedType c a -> TypeContent TRUE any s -> SchemaT (FieldsDefinition c s)
withObject InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject OutputType DataObject {objectFields} = pure objectFields
withObject x _ = failureOnlyObject x

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

deriveOutType :: forall a. (GQLType a, DeriveType OUT a) => Proxy a -> SchemaT ()
deriveOutType _ = deriveType (KindedProxy :: KindedProxy OUT a)

deriveObjectType ::
  DeriveTypeConstraint OUT a =>
  proxy a ->
  SchemaT (TypeDefinition OBJECT CONST)
deriveObjectType proxy = do
  value <- (`mkObjectType` __typeName proxy) <$> deriveObjectFields proxy
  deriveFieldTypes (outputType proxy)
  pure value

mkObjectType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OBJECT CONST
mkObjectType fields typeName = mkType typeName (DataObject [] fields)

failureOnlyObject :: forall c a b. (GQLType a) => KindedType c a -> SchemaT b
failureOnlyObject _ =
  failure
    $ globalErrorMessage
    $ msg (__typeName (Proxy @a)) <> " should have only one nonempty constructor"

deriveFieldValue :: forall f kind a. (DeriveType kind a) => f a -> SchemaT (Maybe (FieldContent TRUE kind CONST))
deriveFieldValue _ = deriveContent (KindedProxy :: KindedProxy k a)

deriveFieldTypes ::
  forall kind a.
  (GQLType a, TypeRep (DeriveType kind) (SchemaT ()) (Rep a), Generic a) =>
  KindedType kind a ->
  SchemaT ()
deriveFieldTypes kinded =
  concatSchemaT
    $ repToValues
    $ genericTo
      (TypeConstraint (`deriveTypeWith` kinded) :: TypeConstraint (DeriveType kind) (SchemaT ()) Proxy)
      (Proxy @a)

type TyContentM kind = (SchemaT (Maybe (FieldContent TRUE kind CONST)))

type TyContent kind = Maybe (FieldContent TRUE kind CONST)

-- Object Fields
deriveTypeContent ::
  forall kind a.
  (TypeRep (DeriveType kind) (TyContentM kind) (Rep a), Generic a, GQLType a) =>
  KindedType kind a ->
  SchemaT (TypeContent TRUE kind CONST)
deriveTypeContent scope =
  unpackMs
    ( genericTo
        (TypeConstraint deriveFieldValue :: TypeConstraint (DeriveType kind) (TyContentM kind) Proxy)
        proxy
    )
    >>= fmap (updateDef proxy) . builder scope
  where
    proxy = Proxy @a

unpackM :: FieldRep (TyContentM k) -> SchemaT (FieldRep (TyContent k))
unpackM FieldRep {fieldValue = v, ..} = do
  fieldValue <- v
  pure $ FieldRep {..}

unpackCons :: ConsRep (TyContentM k) -> SchemaT (ConsRep (TyContent k))
unpackCons ConsRep {consFields = v, ..} = do
  consFields <- traverse unpackM v
  pure $ ConsRep {..}

unpackMs :: [ConsRep (TyContentM k)] -> SchemaT [ConsRep (TyContent k)]
unpackMs = traverse unpackCons

builder ::
  forall kind (a :: *).
  GQLType a =>
  KindedType kind a ->
  [ConsRep (TyContent kind)] ->
  SchemaT (TypeContent TRUE kind CONST)
builder scope [ConsRep {consFields}] = buildObject interfaces scope consFields
  where
    interfaces = sequence $ implements (Proxy @a)
builder scope cons = genericUnion scope cons
  where
    proxy = Proxy @a
    baseName = __typeName proxy
    baseFingerprint = __typeFingerprint proxy
    genericUnion InputType = buildInputUnion (baseName, baseFingerprint)
    genericUnion OutputType = buildUnionType baseName baseFingerprint DataUnion (DataObject [])

class UpdateDef value where
  updateDef :: GQLType a => f a -> value -> value

instance UpdateDef (TypeContent TRUE c CONST) where
  updateDef proxy DataObject {objectFields = fields, ..} =
    DataObject {objectFields = fmap (updateDef proxy) fields, ..}
  updateDef proxy DataInputObject {inputObjectFields = fields} =
    DataInputObject {inputObjectFields = fmap (updateDef proxy) fields, ..}
  updateDef proxy DataInterface {interfaceFields = fields} =
    DataInterface {interfaceFields = fmap (updateDef proxy) fields, ..}
  updateDef proxy (DataEnum enums) = DataEnum $ fmap (updateDef proxy) enums
  updateDef _ x = x

instance GetFieldContent cat => UpdateDef (FieldDefinition cat CONST) where
  updateDef proxy FieldDefinition {fieldName = name, fieldType, fieldContent} =
    FieldDefinition
      { fieldName,
        fieldDescription = lookupDescription (readName fieldName) proxy,
        fieldDirectives = lookupDirectives (readName fieldName) proxy,
        fieldContent = getFieldContent fieldName fieldContent proxy,
        ..
      }
    where
      fieldName = stripNamespace (getNamespace proxy) name

instance UpdateDef (DataEnumValue CONST) where
  updateDef proxy DataEnumValue {enumName = name} =
    DataEnumValue
      { enumName,
        enumDescription = lookupDescription (readTypeName enumName) proxy,
        enumDirectives = lookupDirectives (readTypeName enumName) proxy
      }
    where
      enumName = stripNamespace (getNamespace proxy) name

lookupDescription :: GQLType a => Token -> f a -> Maybe Description
lookupDescription name = (name `M.lookup`) . getDescriptions

lookupDirectives :: GQLType a => Token -> f a -> Directives CONST
lookupDirectives name = fromMaybe [] . (name `M.lookup`) . getDirectives

class GetFieldContent c where
  getFieldContent :: GQLType a => FieldName -> Maybe (FieldContent TRUE c CONST) -> f a -> Maybe (FieldContent TRUE c CONST)

instance GetFieldContent IN where
  getFieldContent name val proxy =
    case name `M.lookup` getFieldContents proxy of
      Just (Just x, _) -> Just (DefaultInputValue x)
      _ -> val

instance GetFieldContent OUT where
  getFieldContent name args proxy =
    case name `M.lookup` getFieldContents proxy of
      Just (_, Just x) -> Just (FieldArgs x)
      _ -> args

buildType :: GQLType a => TypeContent TRUE cat CONST -> f a -> TypeDefinition cat CONST
buildType typeContent proxy =
  TypeDefinition
    { typeName = __typeName proxy,
      typeFingerprint = __typeFingerprint proxy,
      typeDescription = description proxy,
      typeDirectives = [],
      typeContent
    }

updateLib ::
  GQLType a =>
  (f a -> TypeDefinition cat CONST) ->
  SchemaT () ->
  f a ->
  SchemaT ()
updateLib f stack proxy = do
  _ <- stack
  updateSchema (__typeName proxy) (__typeFingerprint proxy) stack f proxy

analyseRep :: TypeName -> [ConsRep (Maybe (FieldContent TRUE kind CONST))] -> ResRep (Maybe (FieldContent TRUE kind CONST))
analyseRep baseName cons =
  ResRep
    { enumCons = fmap consName enumRep,
      unionRef = fieldTypeName <$> concatMap consFields unionRefRep,
      unionRecordRep
    }
  where
    (enumRep, left1) = partition isEmptyConstraint cons
    (unionRefRep, unionRecordRep) = partition (isUnionRef baseName) left1

buildInputUnion ::
  (TypeName, DataFingerprint) ->
  [ConsRep (Maybe (FieldContent TRUE IN CONST))] ->
  SchemaT (TypeContent TRUE IN CONST)
buildInputUnion (baseName, baseFingerprint) cons =
  datatype $ analyseRep baseName cons
  where
    datatype :: ResRep (Maybe (FieldContent TRUE IN CONST)) -> SchemaT (TypeContent TRUE IN CONST)
    datatype ResRep {unionRef = [], unionRecordRep = [], enumCons} = pure $ mkEnumContent enumCons
    datatype ResRep {unionRef, unionRecordRep, enumCons} = DataInputUnion <$> typeMembers
      where
        typeMembers :: SchemaT [UnionMember IN CONST]
        typeMembers = do
          unionMembers <- buildUnions wrapInputObject baseFingerprint unionRecordRep
          pure $ fmap mkUnionMember (unionRef <> unionMembers) <> fmap (`UnionMember` False) enumCons
    wrapInputObject :: (FieldsDefinition IN CONST -> TypeContent TRUE IN CONST)
    wrapInputObject = DataInputObject

buildUnionType ::
  (ELEM LEAF kind ~ TRUE) =>
  TypeName ->
  DataFingerprint ->
  (DataUnion CONST -> TypeContent TRUE kind CONST) ->
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT (TypeContent TRUE kind CONST)
buildUnionType baseName baseFingerprint wrapUnion wrapObject =
  datatype baseName baseFingerprint wrapUnion wrapObject . analyseRep baseName

datatype ::
  (ELEM LEAF kind ~ TRUE) =>
  TypeName ->
  DataFingerprint ->
  (DataUnion CONST -> TypeContent TRUE kind CONST) ->
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  ResRep (Maybe (FieldContent TRUE kind CONST)) ->
  SchemaT (TypeContent TRUE kind CONST)
datatype _ _ _ _ ResRep {unionRef = [], unionRecordRep = [], enumCons} = pure $ mkEnumContent enumCons
datatype baseName baseFingerprint wrapUnion wrapObject ResRep {unionRef, unionRecordRep, enumCons} = wrapUnion . map mkUnionMember <$> typeMembers
  where
    typeMembers = do
      enums <- buildUnionEnum wrapObject baseName baseFingerprint enumCons
      unions <- buildUnions wrapObject baseFingerprint unionRecordRep
      pure (unionRef <> enums <> unions)

buildObject ::
  SchemaT [TypeName] ->
  KindedType kind a ->
  [FieldRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT (TypeContent TRUE kind CONST)
buildObject interfaceM scope consFields = do
  interfaces <- interfaceM
  pure $ wrapWith interfaces scope $ mkFieldsDefinition consFields
  where
    wrapWith :: [TypeName] -> KindedType cat a -> FieldsDefinition cat CONST -> TypeContent TRUE cat CONST
    wrapWith _ InputType = DataInputObject
    wrapWith interfaces OutputType = DataObject interfaces

mkFieldsDefinition :: [FieldRep (Maybe (FieldContent TRUE kind CONST))] -> FieldsDefinition kind CONST
mkFieldsDefinition = unsafeFromFields . fmap fieldByRep

fieldByRep :: FieldRep (Maybe (FieldContent TRUE kind CONST)) -> FieldDefinition kind CONST
fieldByRep FieldRep {fieldSelector, fieldTypeRef, fieldValue} =
  mkField fieldValue fieldSelector fieldTypeRef

buildUnions ::
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  DataFingerprint ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT [TypeName]
buildUnions wrapObject baseFingerprint cons = do
  traverse_ buildURecType cons
  pure $ fmap consName cons
  where
    buildURecType = insertType . buildUnionRecord wrapObject baseFingerprint

insertType ::
  TypeDefinition cat CONST ->
  SchemaT ()
insertType dt@TypeDefinition {typeName, typeFingerprint} =
  updateSchema typeName typeFingerprint (pure ()) (const dt) ()

buildUnionRecord ::
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  DataFingerprint ->
  ConsRep (Maybe (FieldContent TRUE kind CONST)) ->
  TypeDefinition kind CONST
buildUnionRecord wrapObject typeFingerprint ConsRep {consName, consFields} =
  TypeDefinition
    { typeName = consName,
      typeFingerprint,
      typeDescription = Nothing,
      typeDirectives = empty,
      typeContent = wrapObject $ mkFieldsDefinition consFields
    }

buildUnionEnum ::
  (FieldsDefinition cat CONST -> TypeContent TRUE cat CONST) ->
  TypeName ->
  DataFingerprint ->
  [TypeName] ->
  SchemaT [TypeName]
buildUnionEnum wrapObject baseName baseFingerprint enums = updates $> members
  where
    members
      | null enums = []
      | otherwise = [enumTypeWrapperName]
    enumTypeName = baseName <> "Enum"
    enumTypeWrapperName = enumTypeName <> "Object"
    -------------------------
    updates :: SchemaT ()
    updates
      | null enums = pure ()
      | otherwise = do
        buildEnumObject wrapObject enumTypeWrapperName baseFingerprint enumTypeName
        buildEnum enumTypeName baseFingerprint enums

buildEnum :: TypeName -> DataFingerprint -> [TypeName] -> SchemaT ()
buildEnum typeName typeFingerprint tags =
  insertType
    ( TypeDefinition
        { typeDescription = Nothing,
          typeDirectives = empty,
          typeContent = mkEnumContent tags,
          ..
        } ::
        TypeDefinition LEAF CONST
    )

buildEnumObject ::
  (FieldsDefinition cat CONST -> TypeContent TRUE cat CONST) ->
  TypeName ->
  DataFingerprint ->
  TypeName ->
  SchemaT ()
buildEnumObject wrapObject typeName typeFingerprint enumTypeName =
  insertType
    TypeDefinition
      { typeName,
        typeFingerprint,
        typeDescription = Nothing,
        typeDirectives = empty,
        typeContent =
          wrapObject
            $ singleton
            $ mkInputValue "enum" [] enumTypeName
      }
