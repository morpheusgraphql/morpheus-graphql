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
import Control.Monad ((>=>), (>>=), Monad)
import Control.Monad.Fail (fail)
import Data.Foldable (concatMap)
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
    SchemaT,
    TypeConstraint (..),
    TypeRep (..),
    closeWith,
    concatSchemaT,
    fieldTypeName,
    formTypeUpdates,
    genericTo,
    isEmptyConstraint,
    isUnionRef,
    repToValues,
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
    IMPLEMENTABLE,
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
    Show (..),
    null,
    otherwise,
    unzip,
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
  deriveType :: f kind a -> SchemaT m ()

  deriveContent :: f kind a -> Maybe (FieldContent TRUE kind CONST)
  deriveContent _ = Nothing

deriveTypeWith :: DeriveType cat a => f a -> kinded cat b -> SchemaT m ()
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
  deriveContent _ = Just $ FieldArgs $ deriveArgumentDefinition $ Proxy @a
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
  deriveKindedType :: proxy kind a -> SchemaT m ()

-- SCALAR
instance (GQLType a, GQLScalar a) => DeriveKindedType SCALAR a where
  deriveKindedType _ = updateLib scalarType (pure ()) (Proxy @a)
    where
      scalarType :: Proxy a -> SchemaT m (TypeDefinition LEAF CONST)
      scalarType = buildType $ pure $ DataScalar $ scalarValidator (Proxy @a)

-- ENUM
instance DeriveTypeConstraint IN a => DeriveKindedType ENUM a where
  deriveKindedType _ = derivingData $ inputType (Proxy @a)

instance DeriveTypeConstraint IN a => DeriveKindedType INPUT a where
  deriveKindedType _ = derivingData $ inputType (Proxy @a)

instance DeriveTypeConstraint OUT a => DeriveKindedType OUTPUT a where
  deriveKindedType _ = derivingData $ outputType (Proxy @a)

type DeriveTypeConstraint kind a =
  ( GQL_TYPE a,
    TypeRep (DeriveType kind) (Maybe (FieldContent TRUE kind CONST)) (Rep a),
    TypeRep (DeriveType kind) TypeUpdater (Rep a)
  )

instance DeriveTypeConstraint OUT a => DeriveKindedType INTERFACE a where
  deriveKindedType _ = updateLibOUT (buildType intDef) types (Proxy @a)
    where
      intDef = deriveInterfaceContent (Proxy @a)
      types = deriveFieldTypes $ outputType (Proxy @a)

deriveInterfaceContent :: Monad m => f (a :: *) -> SchemaT m (TypeContent TRUE OUT CONST)
deriveInterfaceContent x = fmap DataInterface (deriveObjectFields x)

derivingData ::
  forall kind m a.
  ( TypeRep (DeriveType kind) (Maybe (FieldContent TRUE kind CONST)) (Rep a),
    TypeRep (DeriveType kind) TypeUpdater (Rep a),
    GQLType a,
    Generic a
  ) =>
  KindedType kind a ->
  SchemaT m ()
derivingData kindedType = updateLib (buildType content) types (Proxy @a)
  where
    content = deriveTypeContent kindedType
    types = deriveFieldTypes kindedType

type GQL_TYPE a = (Generic a, GQLType a)

deriveArgumentDefinition :: DeriveTypeConstraint IN a => f a -> SchemaT m (ArgumentsDefinition CONST)
deriveArgumentDefinition = fmap fieldsToArguments . deriveFields . inputType

deriveObjectFields ::
  (TypeRep (DeriveType OUT) (Maybe (FieldContent TRUE OUT CONST)) (Rep a), Generic a, GQLType a) =>
  f a ->
  SchemaT m (FieldsDefinition OUT CONST)
deriveObjectFields = deriveFields . outputType

deriveFields ::
  (GQLType a, TypeRep (DeriveType kind) (Maybe (FieldContent TRUE kind CONST)) (Rep a), Generic a) =>
  KindedType kind a ->
  SchemaT m (FieldsDefinition kind CONST)
deriveFields kindedType = deriveTypeContent kindedType >>= withObject kindedType

withObject :: GQLType a => KindedType c a -> TypeContent TRUE any s -> SchemaT m (FieldsDefinition c s)
withObject InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject OutputType DataObject {objectFields} = pure objectFields
withObject x _ = failureOnlyObject x

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

deriveOutType :: forall a m. (GQLType a, DeriveType OUT a) => Proxy a -> SchemaT m ()
deriveOutType _ = deriveType (KindedProxy :: KindedProxy OUT a)

deriveObjectType ::
  DeriveTypeConstraint OUT a =>
  proxy a ->
  SchemaT m (TypeDefinition OBJECT CONST)
deriveObjectType proxy = do
  value <- (`mkObjectType` __typeName proxy) <$> deriveObjectFields proxy
  deriveFieldTypes (outputType proxy)
  pure value

mkObjectType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OBJECT CONST
mkObjectType fields typeName = mkType typeName (DataObject [] fields)

failureOnlyObject :: forall c a m b. (GQLType a, Applicative m) => KindedType c a -> SchemaT m b
failureOnlyObject _ =
  failure
    $ globalErrorMessage
    $ msg (__typeName (Proxy @a)) <> " should have only one nonempty constructor"

deriveFieldValue :: forall f kind a. (DeriveType kind a) => f a -> Maybe (FieldContent TRUE kind CONST)
deriveFieldValue _ = deriveContent (KindedProxy :: KindedProxy k a)

deriveFieldTypes ::
  forall kind m a.
  (GQLType a, TypeRep (DeriveType kind) (SchemaT m ()) (Rep a), Generic a) =>
  KindedType kind a ->
  SchemaT m ()
deriveFieldTypes kinded =
  concatSchemaT
    $ repToValues
    $ genericTo
      (TypeConstraint (`deriveTypeWith` kinded) :: TypeConstraint (DeriveType kind) (SchemaT m ()) Proxy)
      (Proxy @a)

-- Object Fields
deriveTypeContent ::
  forall kind m a.
  (TypeRep (DeriveType kind) (Maybe (FieldContent TRUE kind CONST)) (Rep a), Generic a, GQLType a) =>
  KindedType kind a ->
  SchemaT m (TypeContent TRUE kind CONST)
deriveTypeContent scope =
  updateD proxy
    $ builder
    $ genericTo
      (TypeConstraint deriveFieldValue :: TypeConstraint (DeriveType kind) (Maybe (FieldContent TRUE kind CONST)) Proxy)
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

updateD :: GQLType a => f a -> (b, [TypeUpdater]) -> SchemaT m b
updateD proxy (v, updates) = formTypeUpdates updates $> updateDef proxy v

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

buildType :: (GQLType a, Monad m) => m (TypeContent TRUE cat CONST) -> f a -> m (TypeDefinition cat CONST)
buildType cType proxy = do
  typeContent <- cType
  pure $
    TypeDefinition
      { typeName = __typeName proxy,
        typeFingerprint = __typeFingerprint proxy,
        typeDescription = description proxy,
        typeDirectives = [],
        typeContent
      }

updateLib ::
  GQLType a =>
  (f a -> SchemaT m (TypeDefinition cat CONST)) ->
  SchemaT m () ->
  f a ->
  SchemaT m ()
updateLib f stack proxy = updateSchema (__typeName proxy) (__typeFingerprint proxy) stack f proxy

updateLibOUT ::
  GQLType a =>
  (f a -> SchemaT m (TypeDefinition cat CONST)) ->
  SchemaT m () ->
  f a ->
  SchemaT m ()
updateLibOUT = updateLib

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
  (TypeName, DataFingerprint) -> [ConsRep (Maybe (FieldContent TRUE IN CONST))] -> (TypeContent TRUE IN CONST, [TypeUpdater])
buildInputUnion (baseName, baseFingerprint) cons =
  datatype
    (analyseRep baseName cons)
  where
    datatype :: ResRep (Maybe (FieldContent TRUE IN CONST)) -> (TypeContent TRUE IN CONST, [TypeUpdater])
    datatype ResRep {unionRef = [], unionRecordRep = [], enumCons} = (mkEnumContent enumCons, [])
    datatype ResRep {unionRef, unionRecordRep, enumCons} =
      (DataInputUnion typeMembers, unionTypes)
      where
        typeMembers :: [UnionMember IN CONST]
        typeMembers = fmap mkUnionMember (unionRef <> unionMembers) <> fmap (`UnionMember` False) enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapInputObject baseFingerprint unionRecordRep
    wrapInputObject :: (FieldsDefinition IN CONST -> TypeContent TRUE IN CONST)
    wrapInputObject = DataInputObject

buildUnionType ::
  (ELEM LEAF kind ~ TRUE) =>
  (TypeName, DataFingerprint) ->
  (DataUnion CONST -> TypeContent TRUE kind CONST) ->
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  (TypeContent TRUE kind CONST, [TypeUpdater])
buildUnionType (baseName, baseFingerprint) wrapUnion wrapObject cons =
  datatype (analyseRep baseName cons)
  where
    --datatype :: ResRep -> (TypeContent TRUE cat, [TypeUpdater])
    datatype ResRep {unionRef = [], unionRecordRep = [], enumCons} = (mkEnumContent enumCons, [])
    datatype ResRep {unionRef, unionRecordRep, enumCons} =
      (wrapUnion (fmap mkUnionMember typeMembers), enumTypes <> unionTypes)
      where
        typeMembers = unionRef <> enumMembers <> unionMembers
        (enumMembers, enumTypes) =
          buildUnionEnum wrapObject baseName baseFingerprint enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapObject baseFingerprint unionRecordRep

buildObject ::
  ([TypeName], [TypeUpdater]) ->
  KindedType kind a ->
  [FieldRep (Maybe (FieldContent TRUE kind CONST))] ->
  (TypeContent TRUE kind CONST, [TypeUpdater])
buildObject (interfaces, interfaceTypes) scope consFields =
  (wrapWith scope (mkFieldsDefinition consFields), interfaceTypes)
  where
    wrapWith :: KindedType cat a -> FieldsDefinition cat CONST -> TypeContent TRUE cat CONST
    wrapWith InputType = DataInputObject
    wrapWith OutputType = DataObject interfaces

mkFieldsDefinition :: [FieldRep (Maybe (FieldContent TRUE kind CONST))] -> FieldsDefinition kind CONST
mkFieldsDefinition = unsafeFromFields . fmap fieldByRep

fieldByRep :: FieldRep (Maybe (FieldContent TRUE kind CONST)) -> FieldDefinition kind CONST
fieldByRep FieldRep {fieldSelector, fieldTypeRef, fieldValue} =
  mkField fieldValue fieldSelector fieldTypeRef

buildUnions ::
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  DataFingerprint ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  ([TypeName], [TypeUpdater])
buildUnions wrapObject baseFingerprint cons = (members, fmap buildURecType cons)
  where
    buildURecType = insertType . buildUnionRecord wrapObject baseFingerprint
    members = fmap consName cons

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
