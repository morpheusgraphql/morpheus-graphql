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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Introspect
  ( Introspect (..),
    DeriveTypeContent (..),
    introspectOUT,
    IntroCon,
    updateLib,
    buildType,
    introspectObjectFields,
    deriveCustomInputObjectType,
    TypeScope (..),
    ProxyRep (..),
    TypeUpdater,
    deriveSchema,
  )
where

import Data.List (partition)
import Data.Map (Map)
-- MORPHEUS

import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( concatUpdates,
    empty,
    failUpdates,
    resolveUpdates,
    singleton,
  )
import Data.Morpheus.Kind
  ( Context (..),
    ENUM,
    GQL_KIND,
    INPUT,
    INTERFACE,
    OUTPUT,
    SCALAR,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( EnumRep (..),
    conNameProxy,
    isRecordProxy,
    selNameProxy,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLType (CUSTOM),
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
    DataFingerprint (..),
    DataUnion,
    FALSE,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldName (..),
    FieldsDefinition,
    IN,
    MUTATION,
    Message,
    OUT,
    QUERY,
    SUBSCRIPTION,
    Schema (..),
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    UnionMember (..),
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
    SubscriptionField (..),
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text
  ( pack,
  )
import GHC.Generics

type IntroCon a = (GQLType a, DeriveTypeContent OUT (CUSTOM a) a)

type IntrospectConstraint m event query mutation subscription =
  ( IntroCon (query (Resolver QUERY event m)),
    IntroCon (mutation (Resolver MUTATION event m)),
    IntroCon (subscription (Resolver SUBSCRIPTION event m))
  )

data ProxyRep (cat :: TypeCategory) a
  = ProxyRep

deriveSchema ::
  forall
    rootResolver
    proxy
    m
    event
    query
    mutation
    subscription.
  (IntrospectConstraint m event query mutation subscription) =>
  proxy (rootResolver m event query mutation subscription) ->
  Eventless (Schema CONST)
deriveSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
  where
    querySchema =
      resolveUpdates (initTypeLib (operatorType fields "Query")) types
      where
        (fields, types) =
          introspectObjectFields
            (Proxy @(CUSTOM (query (Resolver QUERY event m))))
            ("type for query", Proxy @(query (Resolver QUERY event m)))
    ------------------------------
    mutationSchema lib =
      resolveUpdates
        (lib {mutation = maybeOperator fields "Mutation"})
        types
      where
        (fields, types) =
          introspectObjectFields
            (Proxy @(CUSTOM (mutation (Resolver MUTATION event m))))
            ( "type for mutation",
              Proxy @(mutation (Resolver MUTATION event m))
            )
    ------------------------------
    subscriptionSchema lib =
      resolveUpdates
        (lib {subscription = maybeOperator fields "Subscription"})
        types
      where
        (fields, types) =
          introspectObjectFields
            (Proxy @(CUSTOM (subscription (Resolver SUBSCRIPTION event m))))
            ( "type for subscription",
              Proxy @(subscription (Resolver SUBSCRIPTION event m))
            )
    maybeOperator :: FieldsDefinition OUT CONST -> TypeName -> Maybe (TypeDefinition OUT CONST)
    maybeOperator fields
      | null fields = const Nothing
      | otherwise = Just . operatorType fields
    -------------------------------------------------
    operatorType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OUT CONST
    operatorType fields typeName = mkType typeName (DataObject [] fields)

introspectOUT :: forall a. (GQLType a, Introspect OUT a) => Proxy a -> TypeUpdater
introspectOUT _ = introspect (ProxyRep :: ProxyRep OUT a)

instance {-# OVERLAPPABLE #-} (GQLType a, IntrospectKind (KIND a) a) => Introspect cat a where
  introspect _ = introspectKind (Context :: Context (KIND a) a)

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class Introspect (cat :: TypeCategory) a where
  introspect :: proxy cat a -> TypeUpdater
  isObject :: proxy cat a -> Bool
  default isObject :: GQLType a => proxy cat a -> Bool
  isObject _ = isObjectKind (Proxy @a)
  field :: proxy cat a -> FieldName -> FieldDefinition cat CONST
  -----------------------------------------------
  default field ::
    GQLType a =>
    proxy cat a ->
    FieldName ->
    FieldDefinition cat CONST
  field _ = buildField (Proxy @a) Nothing

-- Maybe
instance Introspect cat a => Introspect cat (Maybe a) where
  isObject _ = False
  field _ = toNullable . field (ProxyRep :: ProxyRep cat a)
  introspect _ = introspect (ProxyRep :: ProxyRep cat a)

-- List
instance Introspect cat a => Introspect cat [a] where
  isObject _ = False
  field _ = toListField . field (ProxyRep :: ProxyRep cat a)
  introspect _ = introspect (ProxyRep :: ProxyRep cat a)

-- Tuple
instance Introspect cat (Pair k v) => Introspect cat (k, v) where
  isObject _ = True
  field _ = field (ProxyRep :: ProxyRep cat (Pair k v))
  introspect _ = introspect (ProxyRep :: ProxyRep cat (Pair k v))

-- Set
instance Introspect cat [a] => Introspect cat (Set a) where
  isObject _ = False
  field _ = field (ProxyRep :: ProxyRep cat [a])
  introspect _ = introspect (ProxyRep :: ProxyRep cat [a])

-- Map
instance Introspect cat (MapKind k v Maybe) => Introspect cat (Map k v) where
  isObject _ = True
  field _ = field (ProxyRep :: ProxyRep cat (MapKind k v Maybe))
  introspect _ = introspect (ProxyRep :: ProxyRep cat (MapKind k v Maybe))

-- Resolver : a -> Resolver b
instance (GQLType b, DeriveTypeContent IN 'False a, Introspect OUT b) => Introspect OUT (a -> m b) where
  isObject _ = False
  field _ name = fieldObj {fieldContent = Just (FieldArgs fieldArgs)}
    where
      fieldObj = field (ProxyRep :: ProxyRep OUT b) name
      fieldArgs :: ArgumentsDefinition CONST
      fieldArgs =
        fieldsToArguments
          $ fst
          $ introspectInputObjectFields
            (Proxy :: Proxy 'False)
            (__typeName (Proxy @b), Proxy @a)
  introspect _ = concatUpdates (introspect (ProxyRep :: ProxyRep OUT b) : inputs)
    where
      name = "Arguments for " <> __typeName (Proxy @b)
      inputs :: [TypeUpdater]
      inputs =
        snd $ introspectInputObjectFields (Proxy :: Proxy 'False) (name, Proxy @a)

instance (Introspect OUT a) => Introspect OUT (SubscriptionField a) where
  isObject _ = isObject (ProxyRep :: ProxyRep OUT a)
  field _ = field (ProxyRep :: ProxyRep OUT a)
  introspect _ = introspect (ProxyRep :: ProxyRep OUT a)

--  GQL Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (GQLType b, Introspect cat b) => Introspect cat (Resolver fo e m b) where
  isObject _ = False
  field _ = field (ProxyRep :: ProxyRep cat b)
  introspect _ = introspect (ProxyRep :: ProxyRep cat b)

-- | Introspect With specific Kind: 'kind': object, scalar, enum ...
class IntrospectKind (kind :: GQL_KIND) a where
  introspectKind :: Context kind a -> TypeUpdater -- Generates internal GraphQL Schema

-- SCALAR
instance (GQLType a, GQLScalar a) => IntrospectKind SCALAR a where
  introspectKind _ = updateLib scalarType [] (Proxy @a)
    where
      scalarType = buildType $ DataScalar $ scalarValidator (Proxy @a)

-- ENUM
instance (GQL_TYPE a, EnumRep (Rep a)) => IntrospectKind ENUM a where
  introspectKind _ = updateLib enumType [] (Proxy @a)
    where
      enumType = buildType $ mkEnumContent $ enumTags (Proxy @(Rep a))

instance (GQL_TYPE a, DeriveTypeContent IN (CUSTOM a) a) => IntrospectKind INPUT a where
  introspectKind _ = derivingData (Proxy @a) InputType

instance (GQL_TYPE a, DeriveTypeContent OUT (CUSTOM a) a) => IntrospectKind OUTPUT a where
  introspectKind _ = derivingData (Proxy @a) OutputType

instance (GQL_TYPE a, DeriveTypeContent OUT (CUSTOM a) a) => IntrospectKind INTERFACE a where
  introspectKind _ = updateLibOUT (buildType (DataInterface fields)) types (Proxy @a)
    where
      (fields, types) =
        introspectObjectFields
          (Proxy @(CUSTOM a))
          (baseName, Proxy @a)
      baseName = __typeName (Proxy @a)

derivingData ::
  forall a cat.
  (GQLType a, DeriveTypeContent cat (CUSTOM a) a) =>
  Proxy a ->
  TypeScope cat ->
  TypeUpdater
derivingData _ scope = updateLib (buildType datatypeContent) updates (Proxy @a)
  where
    (datatypeContent, updates) =
      deriveTypeContent
        (Proxy @(CUSTOM a))
        (Proxy @a, unzip $ implements (Proxy @a), scope, baseName, baseFingerprint)
    baseName = __typeName (Proxy @a)
    baseFingerprint = __typeFingerprint (Proxy @a)

type GQL_TYPE a = (Generic a, GQLType a)

deriveCustomInputObjectType ::
  DeriveTypeContent IN TRUE a =>
  (TypeName, proxy a) ->
  TypeUpdater
deriveCustomInputObjectType (name, proxy) =
  concatUpdates (snd $ introspectInputObjectFields (Proxy :: Proxy TRUE) (name, proxy))

introspectInputObjectFields ::
  DeriveTypeContent IN custom a =>
  proxy1 (custom :: Bool) ->
  (TypeName, proxy2 a) ->
  (FieldsDefinition IN CONST, [TypeUpdater])
introspectInputObjectFields p1 (name, proxy) =
  withObject (deriveTypeContent p1 (proxy, ([], []), InputType, "", DataFingerprint "" []))
  where
    withObject (DataInputObject {inputObjectFields}, ts) = (inputObjectFields, ts)
    withObject _ = (empty, [introspectFailure (msg name <> " should have only one nonempty constructor")])

introspectObjectFields ::
  DeriveTypeContent OUT custom a =>
  proxy1 (custom :: Bool) ->
  (TypeName, proxy2 a) ->
  (FieldsDefinition OUT CONST, [TypeUpdater])
introspectObjectFields p1 (name, proxy) =
  withObject (deriveTypeContent p1 (proxy, ([], []), OutputType, "", DataFingerprint "" []))
  where
    withObject (DataObject {objectFields}, ts) = (objectFields, ts)
    withObject _ = (empty, [introspectFailure (msg name <> " should have only one nonempty constructor")])

introspectFailure :: Message -> TypeUpdater
introspectFailure = failUpdates . globalErrorMessage . ("invalid schema: " <>)

-- Object Fields
class DeriveTypeContent cat (custom :: Bool) a where
  deriveTypeContent ::
    proxy1 custom ->
    (proxy2 a, ([TypeName], [TypeUpdater]), TypeScope cat, TypeName, DataFingerprint) ->
    (TypeContent TRUE cat CONST, [TypeUpdater])

instance (TypeRep cat (Rep a), Generic a) => DeriveTypeContent cat FALSE a where
  deriveTypeContent _ (_, interfaces, scope, baseName, baseFingerprint) =
    builder $ typeRep (ProxyRep :: ProxyRep cat (Rep a))
    where
      builder [ConsRep {consFields}] = buildObject interfaces scope consFields
      builder cons = genericUnion scope cons
        where
          genericUnion InputType = buildInputUnion (baseName, baseFingerprint)
          genericUnion OutputType = buildUnionType (baseName, baseFingerprint) DataUnion (DataObject [])

buildField ::
  GQLType a =>
  Proxy a ->
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

buildType :: GQLType a => TypeContent TRUE cat CONST -> Proxy a -> TypeDefinition cat CONST
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
  (Proxy a -> TypeDefinition cat CONST) ->
  [TypeUpdater] ->
  Proxy a ->
  TypeUpdater
updateLib f stack proxy = updateSchema (__typeName proxy) (__typeFingerprint proxy) stack f proxy

updateLibOUT ::
  GQLType a =>
  (Proxy a -> TypeDefinition OUT CONST) ->
  [TypeUpdater] ->
  Proxy a ->
  TypeUpdater
updateLibOUT f stack proxy = updateSchema (__typeName proxy) (__typeFingerprint proxy) stack f proxy

-- NEW AUTOMATIC DERIVATION SYSTEM

data ConsRep cat = ConsRep
  { consName :: TypeName,
    consIsRecord :: Bool,
    consFields :: [FieldRep cat]
  }

data FieldRep cat = FieldRep
  { fieldTypeName :: TypeName,
    fieldData :: FieldDefinition cat CONST,
    fieldTypeUpdater :: TypeUpdater,
    fieldIsObject :: Bool
  }

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
    { enumCons = map consName enumRep,
      unionRef = map fieldTypeName $ concatMap consFields unionRefRep,
      unionRecordRep = unionRecordRep <> map setFieldNames anyonimousUnionRep
    }
  where
    (enumRep, left1) = partition isEmpty cons
    (unionRefRep, left2) = partition (isUnionRef baseName) left1
    (unionRecordRep, anyonimousUnionRep) = partition consIsRecord left2

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
        typeMembers = map mkUnionMember (unionRef <> unionMembers) <> map (`UnionMember` False) enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapInputObject baseFingerprint unionRecordRep
    types = map fieldTypeUpdater $ concatMap consFields cons
    wrapInputObject :: (FieldsDefinition IN CONST -> TypeContent TRUE IN CONST)
    wrapInputObject = DataInputObject

buildUnionType ::
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
      (wrapUnion (map mkUnionMember typeMembers), types <> enumTypes <> unionTypes)
      where
        typeMembers = unionRef <> enumMembers <> unionMembers
        (enumMembers, enumTypes) =
          buildUnionEnum wrapObject baseName baseFingerprint enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapObject baseFingerprint unionRecordRep
    types = map fieldTypeUpdater $ concatMap consFields cons

buildObject :: ([TypeName], [TypeUpdater]) -> TypeScope cat -> [FieldRep cat] -> (TypeContent TRUE cat CONST, [TypeUpdater])
buildObject (interfaces, interfaceTypes) scope consFields =
  ( wrapWith scope fields,
    types <> interfaceTypes
  )
  where
    (fields, types) = buildDataObject consFields
    --- wrap with
    wrapWith :: TypeScope cat -> FieldsDefinition cat CONST -> TypeContent TRUE cat CONST
    wrapWith InputType = DataInputObject
    wrapWith OutputType = DataObject interfaces

buildDataObject :: [FieldRep cat] -> (FieldsDefinition cat CONST, [TypeUpdater])
buildDataObject consFields = (fields, types)
  where
    fields = unsafeFromFields $ map fieldData consFields
    types = map fieldTypeUpdater consFields

buildUnions ::
  (FieldsDefinition cat CONST -> TypeContent TRUE cat CONST) ->
  DataFingerprint ->
  [ConsRep cat] ->
  ([TypeName], [TypeUpdater])
buildUnions wrapObject baseFingerprint cons = (members, map buildURecType cons)
  where
    buildURecType = insertType . buildUnionRecord wrapObject baseFingerprint
    members = map consName cons

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
          $ map fieldData consFields
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
    TypeDefinition
      { typeDescription = Nothing,
        typeDirectives = empty,
        typeContent = mkEnumContent tags,
        ..
      }

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

data TypeScope (cat :: TypeCategory) where
  InputType :: TypeScope IN
  OutputType :: TypeScope OUT

deriving instance Show (TypeScope cat)

deriving instance Eq (TypeScope cat)

deriving instance Ord (TypeScope cat)

--  GENERIC UNION
class TypeRep (cat :: TypeCategory) f where
  typeRep :: ProxyRep cat f -> [ConsRep cat]

instance TypeRep cat f => TypeRep cat (M1 D d f) where
  typeRep _ = typeRep (ProxyRep :: ProxyRep cat f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep cat a, TypeRep cat b) => TypeRep cat (a :+: b) where
  typeRep _ = typeRep (ProxyRep :: ProxyRep cat a) <> typeRep (ProxyRep :: ProxyRep cat b)

instance (ConRep cat f, Constructor c) => TypeRep cat (M1 C c f) where
  typeRep _ =
    [ ConsRep
        { consName = conNameProxy (Proxy @c),
          consFields = conRep (ProxyRep :: ProxyRep cat f),
          consIsRecord = isRecordProxy (Proxy @c)
        }
    ]

class ConRep cat f where
  conRep :: ProxyRep cat f -> [FieldRep cat]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep cat a, ConRep cat b) => ConRep cat (a :*: b) where
  conRep _ = conRep (ProxyRep :: ProxyRep cat a) <> conRep (ProxyRep :: ProxyRep cat b)

instance (Selector s, Introspect cat a) => ConRep cat (M1 S s (Rec0 a)) where
  conRep _ =
    [ FieldRep
        { fieldTypeName = typeConName $ fieldType fieldData,
          fieldData = fieldData,
          fieldTypeUpdater = introspect (ProxyRep :: ProxyRep cat a),
          fieldIsObject = isObject (ProxyRep :: ProxyRep cat a)
        }
    ]
    where
      name = selNameProxy (Proxy @s)
      fieldData = field (ProxyRep :: ProxyRep cat a) name

instance ConRep cat U1 where
  conRep _ = []
