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
  ( conNameProxy,
    isRecordProxy,
    selNameProxy,
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
  ( ArgumentsDefinition (..),
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
    TypeRef (..),
    UnionMember (..),
    VALID,
    fieldsToArguments,
    initTypeLib,
    insertType,
    mkEnumContent,
    mkInputValue,
    mkType,
    mkTypeRef,
    mkUnionMember,
    msg,
    toListField,
    toNullable,
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
import Data.Text
  ( pack,
  )
import GHC.Generics
import Language.Haskell.TH (Exp, Q)
import Prelude
  ( ($),
    (.),
    Bool (..),
    Eq (..),
    Int,
    Show (..),
    fst,
    map,
    null,
    otherwise,
    snd,
    unzip,
    zipWith,
  )

type SchemaConstraints event (m :: * -> *) query mutation subscription =
  ( SchemaConstraint (query (Resolver QUERY event m)),
    SchemaConstraint (mutation (Resolver MUTATION event m)),
    SchemaConstraint (subscription (Resolver SUBSCRIPTION event m))
  )

type SchemaConstraint a =
  ( GQLType a,
    TypeRep OUT (Rep a),
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
class DeriveType (cat :: TypeCategory) a where
  deriveType :: proxy cat a -> TypeUpdater
  field :: FieldName -> proxy cat a -> FieldDefinition cat CONST
  -----------------------------------------------
  default field ::
    GQLType a =>
    FieldName ->
    proxy cat a ->
    FieldDefinition cat CONST
  field fieldName _ = buildField (Proxy @a) Nothing fieldName

deriveTypeWith :: DeriveType cat a => f a -> kinded cat b -> TypeUpdater
deriveTypeWith x = deriveType . setProxyType x

deriveFieldWith :: DeriveType cat a => f a -> FieldName -> kinded cat b -> FieldDefinition cat CONST
deriveFieldWith x name = field name . setProxyType x

-- Maybe
instance DeriveType cat a => DeriveType cat (Maybe a) where
  field name = toNullable . deriveFieldWith (Proxy @a) name
  deriveType = deriveTypeWith (Proxy @a)

-- List
instance DeriveType cat a => DeriveType cat [a] where
  field name = toListField . deriveFieldWith (Proxy @a) name
  deriveType = deriveTypeWith (Proxy @a)

-- Tuple
instance DeriveType cat (Pair k v) => DeriveType cat (k, v) where
  field = deriveFieldWith (Proxy @(Pair k v))
  deriveType = deriveTypeWith (Proxy @(Pair k v))

-- Set
instance DeriveType cat [a] => DeriveType cat (Set a) where
  field = deriveFieldWith (Proxy @[a])
  deriveType = deriveTypeWith (Proxy @[a])

-- Map
instance DeriveType cat (MapKind k v Maybe) => DeriveType cat (Map k v) where
  field = deriveFieldWith (Proxy @(MapKind k v Maybe))
  deriveType = deriveTypeWith (Proxy @(MapKind k v Maybe))

-- Resolver : a -> Resolver b
instance
  ( GQLType b,
    DeriveType OUT b,
    TypeRep IN (Rep a),
    GQLType a,
    Generic a
  ) =>
  DeriveType OUT (a -> m b)
  where
  field name _ = fieldObj {fieldContent = Just (FieldArgs fieldArgs)}
    where
      fieldObj = field name (KindedProxy :: KindedProxy OUT b)
      fieldArgs = fst $ deriveArgumentFields (Proxy @a)
  deriveType _ = concatUpdates (deriveType (KindedProxy :: KindedProxy OUT b) : inputs)
    where
      inputs :: [TypeUpdater]
      inputs = snd $ deriveArgumentFields (Proxy @a)

instance (DeriveType OUT a) => DeriveType OUT (SubscriptionField a) where
  field name _ = field name (KindedProxy :: KindedProxy OUT a)
  deriveType _ = deriveType (KindedProxy :: KindedProxy OUT a)

--  GQL Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (GQLType b, DeriveType cat b) => DeriveType cat (Resolver fo e m b) where
  field name _ = field name (KindedProxy :: KindedProxy cat b)
  deriveType _ = deriveType (KindedProxy :: KindedProxy cat b)

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
instance (GQL_TYPE a, TypeRep IN (Rep a)) => DeriveKindedType ENUM a where
  deriveKindedType _ = derivingData $ inputType (Proxy @a)

instance (GQL_TYPE a, TypeRep IN (Rep a)) => DeriveKindedType INPUT a where
  deriveKindedType _ = derivingData $ inputType (Proxy @a)

instance (GQL_TYPE a, TypeRep OUT (Rep a)) => DeriveKindedType OUTPUT a where
  deriveKindedType _ = derivingData $ outputType (Proxy @a)

instance (GQL_TYPE a, TypeRep OUT (Rep a)) => DeriveKindedType INTERFACE a where
  deriveKindedType _ = updateLibOUT (buildType (DataInterface fields)) types (Proxy @a)
    where
      (fields, types) = deriveObjectFields (Proxy @a)

derivingData ::
  forall cat a.
  (TypeRep cat (Rep a), GQLType a, Generic a) =>
  KindedType cat a ->
  TypeUpdater
derivingData kindedType = updateLib (buildType content) updates (Proxy @a)
  where
    (content, updates) = deriveTypeContent kindedType

type GQL_TYPE a = (Generic a, GQLType a)

deriveArgumentFields :: (TypeRep IN (Rep a), GQLType a, Generic a) => f a -> (ArgumentsDefinition CONST, [TypeUpdater])
deriveArgumentFields = mapFst fieldsToArguments . deriveFields . inputType

deriveObjectFields :: (TypeRep OUT (Rep a), Generic a, GQLType a) => f a -> (FieldsDefinition OUT CONST, [TypeUpdater])
deriveObjectFields = deriveFields . outputType

deriveFields ::
  (GQLType a, TypeRep k (Rep a), Generic a) =>
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
  (TypeRep OUT (Rep a), Generic a, GQLType a) =>
  proxy a ->
  (TypeDefinition OBJECT CONST, [TypeUpdater])
deriveObjectType proxy = (mkObjectType fields (__typeName proxy), types)
  where
    (fields, types) = deriveObjectFields proxy

mkObjectType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OBJECT CONST
mkObjectType fields typeName = mkType typeName (DataObject [] fields)

introspectFailure :: Message -> TypeUpdater
introspectFailure = failUpdates . globalErrorMessage . ("invalid schema: " <>)

-- Object Fields
deriveTypeContent ::
  forall cat a.
  (TypeRep cat (Rep a), Generic a, GQLType a) =>
  KindedType cat a ->
  (TypeContent TRUE cat CONST, [TypeUpdater])
deriveTypeContent scope =
  updateDef proxy
    $ builder
    $ map (stripNamespace (getNamespace (Proxy @a)))
    $ typeRep (KindedProxy :: KindedProxy cat (Rep a))
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

buildField ::
  GQLType a =>
  f a ->
  Maybe (FieldContent TRUE cat CONST) ->
  FieldName ->
  FieldDefinition cat CONST
buildField proxy fieldContent fieldName =
  FieldDefinition
    { fieldType = mkTypeRef (__typeName proxy),
      fieldDescription = Nothing,
      fieldDirectives = empty,
      fieldContent = fieldContent,
      ..
    }

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
updateLibOUT f stack proxy = updateSchema (__typeName proxy) (__typeFingerprint proxy) stack f proxy

-- NEW AUTOMATIC DERIVATION SYSTEM

data ConsRep cat = ConsRep
  { consName :: TypeName,
    consIsRecord :: Bool,
    consFields :: [FieldRep cat]
  }

instance Namespace (ConsRep c) where
  stripNamespace p ConsRep {consFields = fields, ..} = ConsRep {consFields = map (stripNamespace p) fields, ..}

data FieldRep cat = FieldRep
  { fieldTypeName :: TypeName,
    fieldData :: FieldDefinition cat CONST,
    fieldTypeUpdater :: TypeUpdater,
    fieldIsObject :: Bool
  }

instance Namespace (FieldRep c) where
  stripNamespace p FieldRep {fieldData = fields, ..} = FieldRep {fieldData = stripNamespace p fields, ..}

data ResRep cat = ResRep
  { enumCons :: [TypeName],
    unionRef :: [TypeName],
    unionRecordRep :: [ConsRep cat]
  }

isEmpty :: ConsRep cat -> Bool
isEmpty ConsRep {consFields = []} = True
isEmpty _ = False

isUnionRef :: TypeName -> ConsRep cat -> Bool
isUnionRef baseName ConsRep {consName, consFields = [FieldRep {fieldIsObject = True, fieldTypeName}]} =
  consName == baseName <> fieldTypeName
isUnionRef _ _ = False

setFieldNames :: ConsRep cat -> ConsRep cat
setFieldNames cons@ConsRep {consFields} =
  cons
    { consFields = zipWith setFieldName ([0 ..] :: [Int]) consFields
    }
  where
    setFieldName i fieldR@FieldRep {fieldData = fieldD} = fieldR {fieldData = fieldD {fieldName}}
      where
        fieldName = FieldName ("_" <> pack (show i))

analyseRep :: TypeName -> [ConsRep cat] -> ResRep cat
analyseRep baseName cons =
  ResRep
    { enumCons = fmap consName enumRep,
      unionRef = fieldTypeName <$> concatMap consFields unionRefRep,
      unionRecordRep = unionRecordRep <> fmap setFieldNames anonymousUnionRep
    }
  where
    (enumRep, left1) = partition isEmpty cons
    (unionRefRep, left2) = partition (isUnionRef baseName) left1
    (unionRecordRep, anonymousUnionRep) = partition consIsRecord left2

buildInputUnion ::
  (TypeName, DataFingerprint) -> [ConsRep IN] -> (TypeContent TRUE IN CONST, [TypeUpdater])
buildInputUnion (baseName, baseFingerprint) cons =
  datatype
    (analyseRep baseName cons)
  where
    datatype :: ResRep IN -> (TypeContent TRUE IN CONST, [TypeUpdater])
    datatype ResRep {unionRef = [], unionRecordRep = [], enumCons} = (mkEnumContent enumCons, types)
    datatype ResRep {unionRef, unionRecordRep, enumCons} =
      (DataInputUnion typeMembers, types <> unionTypes)
      where
        typeMembers :: [UnionMember IN CONST]
        typeMembers = fmap mkUnionMember (unionRef <> unionMembers) <> fmap (`UnionMember` False) enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapInputObject baseFingerprint unionRecordRep
    types = fieldTypeUpdater <$> concatMap consFields cons
    wrapInputObject :: (FieldsDefinition IN CONST -> TypeContent TRUE IN CONST)
    wrapInputObject = DataInputObject

buildUnionType ::
  (ELEM LEAF cat ~ TRUE) =>
  (TypeName, DataFingerprint) ->
  (DataUnion CONST -> TypeContent TRUE cat CONST) ->
  (FieldsDefinition cat CONST -> TypeContent TRUE cat CONST) ->
  [ConsRep cat] ->
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
    types = fieldTypeUpdater <$> concatMap consFields cons

buildObject :: ([TypeName], [TypeUpdater]) -> KindedType cat a -> [FieldRep cat] -> (TypeContent TRUE cat CONST, [TypeUpdater])
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

buildDataObject :: [FieldRep cat] -> (FieldsDefinition cat CONST, [TypeUpdater])
buildDataObject consFields = (fields, types)
  where
    fields = unsafeFromFields $ fmap fieldData consFields
    types = fmap fieldTypeUpdater consFields

buildUnions ::
  (FieldsDefinition cat CONST -> TypeContent TRUE cat CONST) ->
  DataFingerprint ->
  [ConsRep cat] ->
  ([TypeName], [TypeUpdater])
buildUnions wrapObject baseFingerprint cons = (members, fmap buildURecType cons)
  where
    buildURecType = insertType . buildUnionRecord wrapObject baseFingerprint
    members = fmap consName cons

buildUnionRecord ::
  (FieldsDefinition cat CONST -> TypeContent TRUE cat CONST) -> DataFingerprint -> ConsRep cat -> TypeDefinition cat CONST
buildUnionRecord wrapObject typeFingerprint ConsRep {consName, consFields} =
  TypeDefinition
    { typeName = consName,
      typeFingerprint,
      typeDescription = Nothing,
      typeDirectives = empty,
      typeContent =
        wrapObject
          $ unsafeFromFields
          $ fmap fieldData consFields
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

--  GENERIC UNION
class TypeRep (cat :: TypeCategory) f where
  typeRep :: KindedProxy cat f -> [ConsRep cat]

instance TypeRep cat f => TypeRep cat (M1 D d f) where
  typeRep _ = typeRep (KindedProxy :: KindedProxy cat f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep cat a, TypeRep cat b) => TypeRep cat (a :+: b) where
  typeRep _ = typeRep (KindedProxy :: KindedProxy cat a) <> typeRep (KindedProxy :: KindedProxy cat b)

instance (ConRep cat f, Constructor c) => TypeRep cat (M1 C c f) where
  typeRep _ =
    [ ConsRep
        { consName = conNameProxy (Proxy @c),
          consFields = conRep (KindedProxy :: KindedProxy cat f),
          consIsRecord = isRecordProxy (Proxy @c)
        }
    ]

class ConRep cat f where
  conRep :: KindedProxy cat f -> [FieldRep cat]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep cat a, ConRep cat b) => ConRep cat (a :*: b) where
  conRep _ = conRep (KindedProxy :: KindedProxy cat a) <> conRep (KindedProxy :: KindedProxy cat b)

instance (Selector s, GQLType a, DeriveType cat a) => ConRep cat (M1 S s (Rec0 a)) where
  conRep _ =
    [ FieldRep
        { fieldTypeName = typeConName $ fieldType fieldData,
          fieldData = fieldData,
          fieldTypeUpdater = deriveType (KindedProxy :: KindedProxy cat a),
          fieldIsObject = isObjectKind (Proxy @a)
        }
    ]
    where
      name = selNameProxy (Proxy @s)
      fieldData = field name (KindedProxy :: KindedProxy cat a)

instance ConRep cat U1 where
  conRep _ = []
