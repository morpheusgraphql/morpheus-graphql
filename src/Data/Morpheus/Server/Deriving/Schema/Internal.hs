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

module Data.Morpheus.Server.Deriving.Schema.Internal
  ( KindedProxy (..),
    KindedType (..),
    buildType,
    builder,
    inputType,
    outputType,
    setProxyType,
    unpackMs,
    updateLib,
    mkObjectType,
    UpdateDef (..),
    withObject,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), (>>=), sequence_)
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
    insertType,
    updateSchema,
  )
import Data.Morpheus.Server.Types.Types
  ( MapKind,
    Pair,
  )
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
    mkEnumContent,
    mkField,
    mkInputValue,
    mkType,
    mkUnionMember,
    msg,
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
    map,
    null,
    otherwise,
    sequence,
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

fromSchema :: Eventless (Schema VALID) -> Q Exp
fromSchema Success {} = [|()|]
fromSchema Failure {errors} = fail (show errors)

deriveArgumentDefinition f = fmap fieldsToArguments . f . inputType

withObject :: (GQLType a) => KindedType c a -> TypeContent TRUE any s -> SchemaT (FieldsDefinition c s)
withObject InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject OutputType DataObject {objectFields} = pure objectFields
withObject x _ = failureOnlyObject x

deriveObjectType ::
  (Functor f1, GQLType a) =>
  (f2 a -> f1 (FieldsDefinition OUT CONST)) ->
  f2 a ->
  f1 (TypeDefinition OBJECT CONST)
deriveObjectType f proxy = (`mkObjectType` __typeName proxy) <$> f proxy

mkObjectType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OBJECT CONST
mkObjectType fields typeName = mkType typeName (DataObject [] fields)

failureOnlyObject :: forall c a b. (GQLType a) => KindedType c a -> SchemaT b
failureOnlyObject _ =
  failure
    $ globalErrorMessage
    $ msg (__typeName (Proxy @a)) <> " should have only one nonempty constructor"

type TyContentM kind = (SchemaT (Maybe (FieldContent TRUE kind CONST)))

type TyContent kind = Maybe (FieldContent TRUE kind CONST)

unpackM :: FieldRep (TyContentM k) -> SchemaT (FieldRep (TyContent k))
unpackM FieldRep {..} =
  FieldRep fieldSelector fieldTypeRef fieldIsObject
    <$> fieldValue

unpackCons :: ConsRep (TyContentM k) -> SchemaT (ConsRep (TyContent k))
unpackCons ConsRep {..} = ConsRep consName <$> traverse unpackM consFields

unpackMs :: [ConsRep (TyContentM k)] -> SchemaT [ConsRep (TyContent k)]
unpackMs = traverse unpackCons

builder ::
  forall kind (a :: *).
  GQLType a =>
  KindedType kind a ->
  [ConsRep (TyContent kind)] ->
  SchemaT (TypeContent TRUE kind CONST)
builder scope [ConsRep {consFields}] = buildObj <$> sequence (implements (Proxy @a))
  where
    buildObj interfaces = wrapFields interfaces scope (mkFieldsDefinition consFields)
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

updateLib ::
  GQLType a =>
  (f a -> SchemaT (TypeDefinition cat CONST)) ->
  f a ->
  SchemaT ()
updateLib f proxy =
  updateSchema (__typeName proxy) (__typeFingerprint proxy) f proxy

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
buildInputUnion (baseName, baseFingerprint) =
  mkInputUnionType baseFingerprint . analyseRep baseName

buildUnionType ::
  (ELEM LEAF kind ~ TRUE) =>
  TypeName ->
  DataFingerprint ->
  (DataUnion CONST -> TypeContent TRUE kind CONST) ->
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT (TypeContent TRUE kind CONST)
buildUnionType baseName baseFingerprint wrapUnion wrapObject =
  mkUnionType baseName baseFingerprint wrapUnion wrapObject . analyseRep baseName

mkInputUnionType :: DataFingerprint -> ResRep (Maybe (FieldContent TRUE IN CONST)) -> SchemaT (TypeContent TRUE IN CONST)
mkInputUnionType _ ResRep {unionRef = [], unionRecordRep = [], enumCons} = pure $ mkEnumContent enumCons
mkInputUnionType baseFingerprint ResRep {unionRef, unionRecordRep, enumCons} = DataInputUnion <$> typeMembers
  where
    typeMembers :: SchemaT [UnionMember IN CONST]
    typeMembers = withMembers <$> buildUnions wrapInputObject baseFingerprint unionRecordRep
      where
        withMembers unionMembers = fmap mkUnionMember (unionRef <> unionMembers) <> fmap (`UnionMember` False) enumCons
    wrapInputObject :: (FieldsDefinition IN CONST -> TypeContent TRUE IN CONST)
    wrapInputObject = DataInputObject

mkUnionType ::
  (ELEM LEAF kind ~ TRUE) =>
  TypeName ->
  DataFingerprint ->
  (DataUnion CONST -> TypeContent TRUE kind CONST) ->
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  ResRep (Maybe (FieldContent TRUE kind CONST)) ->
  SchemaT (TypeContent TRUE kind CONST)
mkUnionType _ _ _ _ ResRep {unionRef = [], unionRecordRep = [], enumCons} = pure $ mkEnumContent enumCons
mkUnionType baseName baseFingerprint wrapUnion wrapObject ResRep {unionRef, unionRecordRep, enumCons} = wrapUnion . map mkUnionMember <$> typeMembers
  where
    typeMembers = do
      enums <- buildUnionEnum wrapObject baseName baseFingerprint enumCons
      unions <- buildUnions wrapObject baseFingerprint unionRecordRep
      pure (unionRef <> enums <> unions)

wrapFields :: [TypeName] -> KindedType kind a -> FieldsDefinition kind CONST -> TypeContent TRUE kind CONST
wrapFields _ InputType = DataInputObject
wrapFields interfaces OutputType = DataObject interfaces

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
buildUnions wrapObject baseFingerprint cons =
  traverse_ buildURecType cons $> fmap consName cons
  where
    buildURecType = insertType . buildUnionRecord wrapObject baseFingerprint

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
      | otherwise =
        buildEnumObject wrapObject enumTypeWrapperName baseFingerprint enumTypeName
          *> buildEnum enumTypeName baseFingerprint enums

buildType :: GQLType a => f a -> TypeContent TRUE cat CONST -> TypeDefinition cat CONST
buildType proxy typeContent =
  TypeDefinition
    { typeName = __typeName proxy,
      typeFingerprint = __typeFingerprint proxy,
      typeDescription = description proxy,
      typeDirectives = [],
      typeContent
    }

buildUnionRecord ::
  (FieldsDefinition kind CONST -> TypeContent TRUE kind CONST) ->
  DataFingerprint ->
  ConsRep (Maybe (FieldContent TRUE kind CONST)) ->
  TypeDefinition kind CONST
buildUnionRecord wrapObject typeFingerprint ConsRep {consName, consFields} =
  mkSubType consName typeFingerprint (wrapObject $ mkFieldsDefinition consFields)

buildEnum :: TypeName -> DataFingerprint -> [TypeName] -> SchemaT ()
buildEnum typeName typeFingerprint tags =
  insertType
    ( mkSubType typeName typeFingerprint (mkEnumContent tags) ::
        TypeDefinition LEAF CONST
    )

buildEnumObject ::
  (FieldsDefinition cat CONST -> TypeContent TRUE cat CONST) ->
  TypeName ->
  DataFingerprint ->
  TypeName ->
  SchemaT ()
buildEnumObject wrapObject typeName typeFingerprint enumTypeName =
  insertType $
    mkSubType
      typeName
      typeFingerprint
      ( wrapObject
          $ singleton
          $ mkInputValue "enum" [] enumTypeName
      )

mkSubType :: TypeName -> DataFingerprint -> TypeContent TRUE k CONST -> TypeDefinition k CONST
mkSubType typeName typeFingerprint typeContent =
  TypeDefinition
    { typeName,
      typeFingerprint,
      typeDescription = Nothing,
      typeDirectives = empty,
      typeContent
    }
