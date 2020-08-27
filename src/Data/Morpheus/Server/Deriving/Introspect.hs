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

module Data.Morpheus.Server.Deriving.Introspect
  ( Introspect,
    introspectOUT,
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
    Ord,
    Show (..),
    fst,
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

data ProxyRep (cat :: TypeCategory) a
  = ProxyRep

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
        (query, types) =
          deriveOperationType "Query" $
            Proxy @(query (Resolver QUERY e m))
    ------------------------------
    mutationSchema lib =
      resolveUpdates
        (lib {mutation = optionalType mutation})
        types
      where
        (mutation, types) =
          deriveOperationType "Mutation" $
            Proxy @(mut (Resolver MUTATION e m))
    ------------------------------
    subscriptionSchema lib =
      resolveUpdates
        (lib {subscription = optionalType subscription})
        types
      where
        (subscription, types) =
          deriveOperationType "Subscription" $
            Proxy @(subs (Resolver SUBSCRIPTION e m))

introspectOUT :: forall a. (GQLType a, Introspect OUT a) => Proxy a -> TypeUpdater
introspectOUT _ = introspect (ProxyRep :: ProxyRep OUT a)

instance {-# OVERLAPPABLE #-} (GQLType a, IntrospectKind (KIND a) a) => Introspect cat a where
  introspect _ = introspectKind (Context :: Context (KIND a) a)

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class Introspect (cat :: TypeCategory) a where
  introspect :: proxy cat a -> TypeUpdater
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
  field _ = toNullable . field (ProxyRep :: ProxyRep cat a)
  introspect _ = introspect (ProxyRep :: ProxyRep cat a)

-- List
instance Introspect cat a => Introspect cat [a] where
  field _ = toListField . field (ProxyRep :: ProxyRep cat a)
  introspect _ = introspect (ProxyRep :: ProxyRep cat a)

-- Tuple
instance Introspect cat (Pair k v) => Introspect cat (k, v) where
  field _ = field (ProxyRep :: ProxyRep cat (Pair k v))
  introspect _ = introspect (ProxyRep :: ProxyRep cat (Pair k v))

-- Set
instance Introspect cat [a] => Introspect cat (Set a) where
  field _ = field (ProxyRep :: ProxyRep cat [a])
  introspect _ = introspect (ProxyRep :: ProxyRep cat [a])

-- Map
instance Introspect cat (MapKind k v Maybe) => Introspect cat (Map k v) where
  field _ = field (ProxyRep :: ProxyRep cat (MapKind k v Maybe))
  introspect _ = introspect (ProxyRep :: ProxyRep cat (MapKind k v Maybe))

-- Resolver : a -> Resolver b
instance
  ( GQLType b,
    Introspect OUT b,
    TypeRep IN (Rep a),
    GQLType a,
    Generic a
  ) =>
  Introspect OUT (a -> m b)
  where
  field _ name = fieldObj {fieldContent = Just (FieldArgs fieldArgs)}
    where
      fieldObj = field (ProxyRep :: ProxyRep OUT b) name
      fieldArgs = fst $ deriveArgumentFields (Proxy @a)
  introspect _ = concatUpdates (introspect (ProxyRep :: ProxyRep OUT b) : inputs)
    where
      inputs :: [TypeUpdater]
      inputs = snd $ deriveArgumentFields (Proxy @a)

instance (Introspect OUT a) => Introspect OUT (SubscriptionField a) where
  field _ = field (ProxyRep :: ProxyRep OUT a)
  introspect _ = introspect (ProxyRep :: ProxyRep OUT a)

--  GQL Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (GQLType b, Introspect cat b) => Introspect cat (Resolver fo e m b) where
  field _ = field (ProxyRep :: ProxyRep cat b)
  introspect _ = introspect (ProxyRep :: ProxyRep cat b)

-- | Introspect With specific Kind: 'kind': object, scalar, enum ...
class IntrospectKind (kind :: GQL_KIND) a where
  introspectKind :: Context kind a -> TypeUpdater -- Generates internal GraphQL Schema

-- SCALAR
instance (GQLType a, GQLScalar a) => IntrospectKind SCALAR a where
  introspectKind _ = updateLib scalarType [] (Proxy @a)
    where
      scalarType :: Proxy a -> TypeDefinition LEAF CONST
      scalarType = buildType $ DataScalar $ scalarValidator (Proxy @a)

-- ENUM
instance (GQL_TYPE a, EnumRep (Rep a)) => IntrospectKind ENUM a where
  introspectKind _ = updateLib enumType [] (Proxy @a)
    where
      enumType :: Proxy a -> TypeDefinition LEAF CONST
      enumType = buildType $ mkEnumContent $ enumTags (Proxy @(Rep a))

instance (GQL_TYPE a, TypeRep IN (Rep a)) => IntrospectKind INPUT a where
  introspectKind _ = derivingData (Proxy @a) InputType

instance (GQL_TYPE a, TypeRep OUT (Rep a)) => IntrospectKind OUTPUT a where
  introspectKind _ = derivingData (Proxy @a) OutputType

instance (GQL_TYPE a, TypeRep OUT (Rep a)) => IntrospectKind INTERFACE a where
  introspectKind _ = updateLibOUT (buildType (DataInterface fields)) types (Proxy @a)
    where
      (fields, types) = deriveObjectFields (Proxy @a)

derivingData ::
  forall a f cat.
  (TypeRep cat (Rep a), GQLType a, Generic a) =>
  f a ->
  TypeScope cat ->
  TypeUpdater
derivingData proxy scope = updateLib (buildType content) updates proxy
  where
    (content, updates) = deriveTypeContent proxy scope

type GQL_TYPE a = (Generic a, GQLType a)

deriveArgumentFields ::
  ( TypeRep IN (Rep a),
    GQLType a,
    Generic a
  ) =>
  f a ->
  (ArgumentsDefinition CONST, [TypeUpdater])
deriveArgumentFields proxy = withObject (deriveTypeContent proxy InputType)
  where
    withObject (DataInputObject {inputObjectFields}, ts) = (fieldsToArguments inputObjectFields, ts)
    withObject _ =
      ( empty,
        [ introspectFailure ("ArgumentType: " <> msg (__typeName proxy) <> " should have only one nonempty constructor")
        ]
      )

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

deriveOperationType ::
  (TypeRep OUT (Rep a), Generic a, GQLType a) =>
  TypeName ->
  proxy a ->
  (TypeDefinition OBJECT CONST, [TypeUpdater])
deriveOperationType name proxy = (mkOperationType fields name, types)
  where
    (fields, types) = deriveObjectFields proxy

mkOperationType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OBJECT CONST
mkOperationType fields typeName = mkType typeName (DataObject [] fields)

deriveObjectFields ::
  (TypeRep OUT (Rep a), Generic a, GQLType a) =>
  f a ->
  (FieldsDefinition OUT CONST, [TypeUpdater])
deriveObjectFields proxy = withObject (deriveTypeContent proxy OutputType)
  where
    withObject (DataObject {objectFields}, ts) = (objectFields, ts)
    withObject _ = (empty, [introspectFailure (msg (__typeName proxy) <> " should have only one nonempty constructor")])

introspectFailure :: Message -> TypeUpdater
introspectFailure = failUpdates . globalErrorMessage . ("invalid schema: " <>)

-- Object Fields

deriveTypeContent ::
  forall f cat a.
  (TypeRep cat (Rep a), Generic a, GQLType a) =>
  f a ->
  TypeScope cat ->
  (TypeContent TRUE cat CONST, [TypeUpdater])
deriveTypeContent proxy scope =
  updateContentWith proxy
    $ builder
    $ stripNamespace (hasNamespace proxy)
      <$> typeRep (ProxyRep :: ProxyRep cat (Rep a))
  where
    builder [ConsRep {consFields}] = buildObject interfaces scope consFields
      where
        interfaces = unzip (implements proxy)
    builder cons = genericUnion scope cons
      where
        baseName = __typeName proxy
        baseFingerprint = __typeFingerprint proxy
        genericUnion InputType = buildInputUnion (baseName, baseFingerprint)
        genericUnion OutputType = buildUnionType (baseName, baseFingerprint) DataUnion (DataObject [])

updateContentWith :: (GQLType a) => f a -> (TypeContent TRUE c CONST, x) -> (TypeContent TRUE c CONST, x)
updateContentWith proxy (DataObject {objectFields = fields, ..}, x) =
  (DataObject {objectFields = fmap (updateFieldMeta proxy) fields, ..}, x)
updateContentWith proxy (DataInputObject {inputObjectFields = fields}, x) =
  (DataInputObject {inputObjectFields = fmap (updateFieldMeta proxy) fields, ..}, x)
updateContentWith proxy (DataInterface {interfaceFields = fields}, x) =
  (DataInterface {interfaceFields = fmap (updateFieldMeta proxy) fields, ..}, x)
updateContentWith proxy (DataEnum enums, x) =
  (DataEnum $ fmap (updateEnumValue proxy) enums, x)
updateContentWith _ x = x

updateEnumValue :: GQLType a => f a -> DataEnumValue s -> DataEnumValue s
updateEnumValue proxy enum =
  enum
    { enumDescription = getDescription (readTypeName $ enumName enum) proxy
    }

updateFieldMeta :: (GQLType a, GetFieldContent cat) => f a -> FieldDefinition cat CONST -> FieldDefinition cat CONST
updateFieldMeta proxy f =
  f
    { fieldDescription = getDescription (readName $ fieldName f) proxy,
      fieldDirectives = getDirectives (fieldName f) proxy,
      fieldContent = getFieldContent (fieldName f) (fieldContent f) proxy
    }

getDescription :: GQLType a => Token -> f a -> Maybe Description
getDescription name = (name `M.lookup`) . getDescriptions

getDirectives :: GQLType a => FieldName -> f a -> Directives CONST
getDirectives name = fromMaybe [] . (name `M.lookup`) . getFieldDirectives

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
  stripNamespace ns ConsRep {consFields = fields, ..} = ConsRep {consFields = fmap (stripNamespace ns) fields, ..}

data FieldRep cat = FieldRep
  { fieldTypeName :: TypeName,
    fieldData :: FieldDefinition cat CONST,
    fieldTypeUpdater :: TypeUpdater,
    fieldIsObject :: Bool
  }

instance Namespace (FieldRep c) where
  stripNamespace ns f = f {fieldData = stripNamespace ns (fieldData f)}

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

instance (Selector s, GQLType a, Introspect cat a) => ConRep cat (M1 S s (Rec0 a)) where
  conRep _ =
    [ FieldRep
        { fieldTypeName = typeConName $ fieldType fieldData,
          fieldData = fieldData,
          fieldTypeUpdater = introspect (ProxyRep :: ProxyRep cat a),
          fieldIsObject = isObjectKind (Proxy @a)
        }
    ]
    where
      name = selNameProxy (Proxy @s)
      fieldData = field (ProxyRep :: ProxyRep cat a) name

instance ConRep cat U1 where
  conRep _ = []
