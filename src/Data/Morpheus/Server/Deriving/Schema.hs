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
  ( DeriveType,
    deriveOutType,
    SchemaConstraints,
    TypeUpdater,
    deriveSchema,
    compileTimeSchemaValidation,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), (>>=))
import Control.Monad.Fail (fail)
import Data.Foldable (concatMap)
import Data.Functor ((<$>), Functor (..))
import Data.List (partition)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    Namespace (..),
    concatUpdates,
    empty,
    failUpdates,
    mapFst,
    resolveUpdates,
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
    enumerateFieldNames,
    genericTo,
    isEmptyConstraint,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeUpdater,
  )
import Data.Morpheus.Server.Types.Types
  ( MapKind,
    Pair,
  )
import Data.Morpheus.Types.GQLScalar (GQLScalar (..))
import Data.Morpheus.Types.Internal.AST
  ( CONST,
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
    TypeRef (..),
    TypeRef (..),
    UnionMember (..),
    VALID,
    fieldsToArguments,
    initTypeLib,
    insertType,
    mkEnumContent,
    mkField,
    mkInputValue,
    mkType,
    mkUnionMember,
    msg,
    unsafeFromFields,
    updateSchema,
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
import GHC.Generics (Generic, Rep)
import Language.Haskell.TH (Exp, Q)
import Prelude
  ( ($),
    (.),
    Bool (..),
    Eq (..),
    Show (..),
    fst,
    null,
    otherwise,
    snd,
    unzip,
  )

type SchemaConstraints event (m :: * -> *) query mutation subscription =
  ( SchemaConstraint (query (Resolver QUERY event m)),
    SchemaConstraint (mutation (Resolver MUTATION event m)),
    SchemaConstraint (subscription (Resolver SUBSCRIPTION event m))
  )

type SchemaConstraint a =
  ( GQLType a,
    TypeRep (DeriveType OUT) (FieldValue OUT) (Rep a),
    Generic a
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
      resolveUpdates (initTypeLib query) types
      where
        (query, types) = deriveObjectType $ Proxy @(query (Resolver QUERY e m))
    ------------------------------
    mutationSchema lib =
      resolveUpdates
        (lib {mutation = optionalType mutation})
        types
      where
        (mutation, types) = deriveObjectType $ Proxy @(mut (Resolver MUTATION e m))
    ------------------------------
    subscriptionSchema lib =
      resolveUpdates
        (lib {subscription = optionalType subscription})
        types
      where
        (subscription, types) = deriveObjectType $ Proxy @(subs (Resolver SUBSCRIPTION e m))

instance {-# OVERLAPPABLE #-} (GQLType a, DeriveKindedType (KIND a) a) => DeriveType cat a where
  deriveType _ = deriveKindedType (KindedProxy :: KindedProxy (KIND a) a)

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class DeriveType (kind :: TypeCategory) (a :: *) where
  deriveType :: f kind a -> TypeUpdater

  deriveContent :: f kind a -> Maybe (FieldContent TRUE kind CONST)
  deriveContent _ = Nothing

deriveTypeWith :: DeriveType cat a => f a -> kinded cat b -> TypeUpdater
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

data FieldValue c = FieldValue
  { fieldValueContent :: Maybe (FieldContent TRUE c CONST),
    fieldTypes :: TypeUpdater
  }

-- Resolver : a -> Resolver b
instance
  ( GQLType b,
    DeriveType OUT b,
    TypeRep (DeriveType IN) (FieldValue IN) (Rep a),
    GQLType a,
    Generic a
  ) =>
  DeriveType OUT (a -> m b)
  where
  deriveContent _ = Just $ fst $ deriveArgumentFields $ Proxy @a
  deriveType _ = concatUpdates (deriveType (KindedProxy :: KindedProxy OUT b) : inputs)
    where
      inputs :: [TypeUpdater]
      inputs = snd $ deriveArgumentFields (Proxy @a)

instance (DeriveType OUT a) => DeriveType OUT (SubscriptionField a) where
  deriveType _ = deriveType (KindedProxy :: KindedProxy OUT a)

--  GQL Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (DeriveType cat b) => DeriveType cat (Resolver fo e m b) where
  deriveType = deriveTypeWith (Proxy @b)

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType (kind :: GQL_KIND) a where
  deriveKindedType :: proxy kind a -> TypeUpdater -- Generates internal GraphQL Schema

-- SCALAR
instance (GQLType a, GQLScalar a) => DeriveKindedType SCALAR a where
  deriveKindedType _ = updateLib scalarType [] (Proxy @a)
    where
      scalarType :: Proxy a -> TypeDefinition LEAF CONST
      scalarType = buildType $ DataScalar $ scalarValidator (Proxy @a)

-- ENUM
instance (GQL_TYPE a, TypeRep (DeriveType IN) (FieldValue IN) (Rep a)) => DeriveKindedType ENUM a where
  deriveKindedType _ = derivingData $ inputType (Proxy @a)

instance (GQL_TYPE a, TypeRep (DeriveType IN) (FieldValue IN) (Rep a)) => DeriveKindedType INPUT a where
  deriveKindedType _ = derivingData $ inputType (Proxy @a)

instance (GQL_TYPE a, TypeRep (DeriveType OUT) (FieldValue OUT) (Rep a)) => DeriveKindedType OUTPUT a where
  deriveKindedType _ = derivingData $ outputType (Proxy @a)

instance (GQL_TYPE a, TypeRep (DeriveType OUT) (FieldValue OUT) (Rep a)) => DeriveKindedType INTERFACE a where
  deriveKindedType _ = updateLibOUT (buildType (DataInterface fields)) types (Proxy @a)
    where
      (fields, types) = deriveObjectFields (Proxy @a)

derivingData ::
  forall cat a.
  (TypeRep (DeriveType cat) (FieldValue cat) (Rep a), GQLType a, Generic a) =>
  KindedType cat a ->
  TypeUpdater
derivingData kindedType = updateLib (buildType content) updates (Proxy @a)
  where
    (content, updates) = deriveTypeContent kindedType

type GQL_TYPE a = (Generic a, GQLType a)

deriveArgumentFields :: (TypeRep (DeriveType IN) (FieldValue IN) (Rep a), GQLType a, Generic a) => f a -> (FieldContent TRUE OUT CONST, [TypeUpdater])
deriveArgumentFields = mapFst (FieldArgs . fieldsToArguments) . deriveFields . inputType

deriveObjectFields :: (TypeRep (DeriveType OUT) (FieldValue OUT) (Rep a), Generic a, GQLType a) => f a -> (FieldsDefinition OUT CONST, [TypeUpdater])
deriveObjectFields = deriveFields . outputType

deriveFields ::
  (GQLType a, TypeRep (DeriveType k) (FieldValue k) (Rep a), Generic a) =>
  KindedType k a ->
  (FieldsDefinition k CONST, [TypeUpdater])
deriveFields kindedType = withObject kindedType (deriveTypeContent kindedType)

withObject ::
  forall a c any s.
  (GQLType a) =>
  KindedType c a ->
  (TypeContent TRUE any s, [TypeUpdater]) ->
  (FieldsDefinition c s, [TypeUpdater])
withObject InputType (DataInputObject {inputObjectFields}, ts) = (inputObjectFields, ts)
withObject OutputType (DataObject {objectFields}, ts) = (objectFields, ts)
withObject _ _ = (empty, [introspectFailure (msg (__typeName (Proxy @a)) <> " should have only one nonempty constructor")])

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

deriveOutType :: forall a. (GQLType a, DeriveType OUT a) => Proxy a -> TypeUpdater
deriveOutType _ = deriveType (KindedProxy :: KindedProxy OUT a)

deriveObjectType ::
  (TypeRep (DeriveType OUT) (FieldValue OUT) (Rep a), Generic a, GQLType a) =>
  proxy a ->
  (TypeDefinition OBJECT CONST, [TypeUpdater])
deriveObjectType proxy = (mkObjectType fields (__typeName proxy), types)
  where
    (fields, types) = deriveObjectFields proxy

mkObjectType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OBJECT CONST
mkObjectType fields typeName = mkType typeName (DataObject [] fields)

introspectFailure :: Message -> TypeUpdater
introspectFailure = failUpdates . globalErrorMessage . ("invalid schema: " <>)

deriveFieldValue :: forall f k a. (DeriveType k a) => f a -> FieldValue k
deriveFieldValue _ =
  FieldValue
    { fieldValueContent = deriveContent (KindedProxy :: KindedProxy k a),
      fieldTypes = deriveType (KindedProxy :: KindedProxy k a)
    }

-- Object Fields
deriveTypeContent ::
  forall k a.
  (TypeRep (DeriveType k) (FieldValue k) (Rep a), Generic a, GQLType a) =>
  KindedType k a ->
  (TypeContent TRUE k CONST, [TypeUpdater])
deriveTypeContent scope =
  updateDef proxy
    $ builder
    $ genericTo
      (TypeConstraint deriveFieldValue :: TypeConstraint (DeriveType cat) (FieldValue cat) Proxy)
      (Proxy @a)
  where
    proxy = Proxy @a
    builder [ConsRep {consFields}] = buildObject interfaces scope consFields
      where
        interfaces = unzip (implements proxy)
    builder cons = genericUnion scope cons
      where
        baseName = __typeName proxy
        baseFingerprint = __typeFingerprint proxy
        genericUnion InputType = buildInputUnion (baseName, baseFingerprint)
        genericUnion OutputType = buildUnionType (baseName, baseFingerprint) DataUnion (DataObject [])

class UpdateDef value where
  updateDef :: GQLType a => f a -> value -> value

instance UpdateDef a => UpdateDef (a, [TypeUpdater]) where
  updateDef proxy (x, y) = (updateDef proxy x, y)

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
  [TypeUpdater] ->
  f a ->
  TypeUpdater
updateLib f stack proxy = updateSchema (__typeName proxy) (__typeFingerprint proxy) stack f proxy

updateLibOUT ::
  GQLType a =>
  (f a -> TypeDefinition OUT CONST) ->
  [TypeUpdater] ->
  f a ->
  TypeUpdater
updateLibOUT = updateLib

fieldTypeName :: FieldRep (FieldValue k) -> TypeName
fieldTypeName = typeConName . fieldTypeRef

isUnionRef :: TypeName -> ConsRep (FieldValue k) -> Bool
isUnionRef baseName ConsRep {consName, consFields = [fieldRep@FieldRep {fieldIsObject = True}]} =
  consName == baseName <> fieldTypeName fieldRep
isUnionRef _ _ = False

analyseRep :: TypeName -> [ConsRep (FieldValue cat)] -> ResRep (FieldValue cat)
analyseRep baseName cons =
  ResRep
    { enumCons = fmap consName enumRep,
      unionRef = fieldTypeName <$> concatMap consFields unionRefRep,
      unionRecordRep = unionRecordRep <> fmap enumerateFieldNames anonymousUnionRep
    }
  where
    (enumRep, left1) = partition isEmptyConstraint cons
    (unionRefRep, left2) = partition (isUnionRef baseName) left1
    (unionRecordRep, anonymousUnionRep) = partition consIsRecord left2

buildInputUnion ::
  (TypeName, DataFingerprint) -> [ConsRep (FieldValue IN)] -> (TypeContent TRUE IN CONST, [TypeUpdater])
buildInputUnion (baseName, baseFingerprint) cons =
  datatype
    (analyseRep baseName cons)
  where
    datatype :: ResRep (FieldValue IN) -> (TypeContent TRUE IN CONST, [TypeUpdater])
    datatype ResRep {unionRef = [], unionRecordRep = [], enumCons} = (mkEnumContent enumCons, types)
    datatype ResRep {unionRef, unionRecordRep, enumCons} =
      (DataInputUnion typeMembers, types <> unionTypes)
      where
        typeMembers :: [UnionMember IN CONST]
        typeMembers = fmap mkUnionMember (unionRef <> unionMembers) <> fmap (`UnionMember` False) enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapInputObject baseFingerprint unionRecordRep
    types = fieldTypes . fieldValue <$> concatMap consFields cons
    wrapInputObject :: (FieldsDefinition IN CONST -> TypeContent TRUE IN CONST)
    wrapInputObject = DataInputObject

buildUnionType ::
  (ELEM LEAF cat ~ TRUE) =>
  (TypeName, DataFingerprint) ->
  (DataUnion CONST -> TypeContent TRUE cat CONST) ->
  (FieldsDefinition cat CONST -> TypeContent TRUE cat CONST) ->
  [ConsRep (FieldValue cat)] ->
  (TypeContent TRUE cat CONST, [TypeUpdater])
buildUnionType (baseName, baseFingerprint) wrapUnion wrapObject cons =
  datatype
    (analyseRep baseName cons)
  where
    --datatype :: ResRep -> (TypeContent TRUE cat, [TypeUpdater])
    datatype ResRep {unionRef = [], unionRecordRep = [], enumCons} = (mkEnumContent enumCons, types)
    datatype ResRep {unionRef, unionRecordRep, enumCons} =
      (wrapUnion (fmap mkUnionMember typeMembers), types <> enumTypes <> unionTypes)
      where
        typeMembers = unionRef <> enumMembers <> unionMembers
        (enumMembers, enumTypes) =
          buildUnionEnum wrapObject baseName baseFingerprint enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapObject baseFingerprint unionRecordRep
    types = fieldTypes . fieldValue <$> concatMap consFields cons

buildObject ::
  ([TypeName], [TypeUpdater]) ->
  KindedType cat a ->
  [FieldRep (FieldValue cat)] ->
  (TypeContent TRUE cat CONST, [TypeUpdater])
buildObject (interfaces, interfaceTypes) scope consFields =
  ( wrapWith scope fields,
    types <> interfaceTypes
  )
  where
    (fields, types) = buildDataObject consFields
    --- wrap with
    wrapWith :: KindedType cat a -> FieldsDefinition cat CONST -> TypeContent TRUE cat CONST
    wrapWith InputType = DataInputObject
    wrapWith OutputType = DataObject interfaces

buildDataObject :: [FieldRep (FieldValue cat)] -> (FieldsDefinition cat CONST, [TypeUpdater])
buildDataObject consFields =
  ( mkFieldsDefinition consFields,
    fmap (fieldTypes . fieldValue) consFields
  )

mkFieldsDefinition :: [FieldRep (FieldValue kind)] -> FieldsDefinition kind CONST
mkFieldsDefinition = unsafeFromFields . fmap fieldByRep

fieldByRep :: FieldRep (FieldValue kind) -> FieldDefinition kind CONST
fieldByRep FieldRep {fieldSelector, fieldTypeRef, fieldValue = FieldValue {fieldValueContent}} =
  mkField fieldValueContent fieldSelector fieldTypeRef

buildUnions ::
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  DataFingerprint ->
  [ConsRep (FieldValue kind)] ->
  ([TypeName], [TypeUpdater])
buildUnions wrapObject baseFingerprint cons = (members, fmap buildURecType cons)
  where
    buildURecType = insertType . buildUnionRecord wrapObject baseFingerprint
    members = fmap consName cons

buildUnionRecord ::
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  DataFingerprint ->
  ConsRep (FieldValue kind) ->
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
  ([TypeName], [TypeUpdater])
buildUnionEnum wrapObject baseName baseFingerprint enums = (members, updates)
  where
    members
      | null enums = []
      | otherwise = [enumTypeWrapperName]
    enumTypeName = baseName <> "Enum"
    enumTypeWrapperName = enumTypeName <> "Object"
    -------------------------
    updates :: [TypeUpdater]
    updates
      | null enums =
        []
      | otherwise =
        [ buildEnumObject
            wrapObject
            enumTypeWrapperName
            baseFingerprint
            enumTypeName,
          buildEnum enumTypeName baseFingerprint enums
        ]

buildEnum :: TypeName -> DataFingerprint -> [TypeName] -> TypeUpdater
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
  TypeUpdater
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
