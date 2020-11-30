{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ( KindedType (..),
    builder,
    unpackMs,
    UpdateDef (..),
    withObject,
    TyContentM,
    asObjectType,
    fromSchema,
    updateByContent,
  )
where

-- MORPHEUS
import Data.List (partition)
import qualified Data.Map as M
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    singleton,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    FieldRep (..),
    ResRep (..),
    fieldTypeName,
    isEmptyConstraint,
    isUnionRef,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeData (..),
    __typeData,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    insertType,
    updateSchema,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataEnumValue (..),
    Description,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldName (..),
    FieldsDefinition,
    IN,
    LEAF,
    OBJECT,
    OUT,
    Schema (..),
    TRUE,
    Token,
    TypeCategory (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    UnionMember (..),
    VALID,
    mkEnumContent,
    mkField,
    mkNullaryMember,
    mkType,
    mkTypeRef,
    mkUnionMember,
    msg,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Morpheus.Utils.Kinded
  ( CategoryValue (..),
    KindedType (..),
    outputType,
  )
import Language.Haskell.TH (Exp, Q)
import Relude

fromSchema :: Eventless (Schema VALID) -> Q Exp
fromSchema Success {} = [|()|]
fromSchema Failure {errors} = fail (show errors)

withObject :: (GQLType a, CategoryValue c) => KindedType c a -> TypeContent TRUE any s -> SchemaT (FieldsDefinition c s)
withObject InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject OutputType DataObject {objectFields} = pure objectFields
withObject x _ = failureOnlyObject x

asObjectType ::
  (GQLType a) =>
  (f2 a -> SchemaT (FieldsDefinition OUT CONST)) ->
  f2 a ->
  SchemaT (TypeDefinition OBJECT CONST)
asObjectType f proxy = (`mkObjectType` gqlTypeName (__typeData (outputType proxy))) <$> f proxy

mkObjectType :: FieldsDefinition OUT CONST -> TypeName -> TypeDefinition OBJECT CONST
mkObjectType fields typeName = mkType typeName (DataObject [] fields)

failureOnlyObject :: forall (c :: TypeCategory) a b. (GQLType a, CategoryValue c) => KindedType c a -> SchemaT b
failureOnlyObject proxy =
  failure
    $ globalErrorMessage
    $ msg (gqlTypeName $ __typeData proxy) <> " should have only one nonempty constructor"

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
  (GQLType a, CategoryValue kind) =>
  KindedType kind a ->
  [ConsRep (TyContent kind)] ->
  SchemaT (TypeContent TRUE kind CONST)
builder scope [ConsRep {consFields}] = buildObj <$> sequence (implements scope)
  where
    buildObj interfaces = wrapFields interfaces scope (mkFieldsDefinition consFields)
builder scope cons = genericUnion cons
  where
    typeData = __typeData scope
    genericUnion = mkUnionType scope . analyseRep (gqlTypeName typeData)

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
  updateDef proxy FieldDefinition {fieldName, fieldType, fieldContent} =
    FieldDefinition
      { fieldName,
        fieldDescription = lookupDescription (readName fieldName) proxy,
        fieldDirectives = lookupDirectives (readName fieldName) proxy,
        fieldContent = getFieldContent fieldName fieldContent proxy,
        ..
      }

instance UpdateDef (DataEnumValue CONST) where
  updateDef proxy DataEnumValue {enumName} =
    DataEnumValue
      { enumName,
        enumDescription = lookupDescription (readTypeName enumName) proxy,
        enumDirectives = lookupDirectives (readTypeName enumName) proxy
      }

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

updateByContent ::
  (GQLType a, CategoryValue kind) =>
  (f kind a -> SchemaT (TypeContent TRUE kind CONST)) ->
  f kind a ->
  SchemaT ()
updateByContent f proxy =
  updateSchema
    (gqlFingerprint $ __typeData proxy)
    deriveD
    proxy
  where
    deriveD =
      fmap
        ( TypeDefinition
            (description proxy)
            (gqlTypeName (__typeData proxy))
            []
        )
        . f

analyseRep :: TypeName -> [ConsRep (Maybe (FieldContent TRUE kind CONST))] -> ResRep (Maybe (FieldContent TRUE kind CONST))
analyseRep baseName cons
  | all isEmptyConstraint cons = EnumRep {enumCons = consName <$> cons}
  | otherwise =
    ResRep
      { unionRef = fieldTypeName <$> concatMap consFields unionRefRep,
        unionCons
      }
  where
    (unionRefRep, unionCons) = partition (isUnionRef baseName) cons

mkUnionType ::
  KindedType kind a ->
  ResRep (Maybe (FieldContent TRUE kind CONST)) ->
  SchemaT (TypeContent TRUE kind CONST)
mkUnionType InputType EnumRep {enumCons} = pure $ mkEnumContent enumCons
mkUnionType OutputType EnumRep {enumCons} = pure $ mkEnumContent enumCons
mkUnionType InputType ResRep {unionRef, unionCons} = DataInputUnion <$> typeMembers
  where
    (nullaries, cons) = partition isEmptyConstraint unionCons
    nullaryMembers :: [UnionMember IN CONST]
    nullaryMembers = mkNullaryMember . consName <$> nullaries
    typeMembers :: SchemaT [UnionMember IN CONST]
    typeMembers = (<> nullaryMembers) . withRefs <$> buildUnions cons
      where
        withRefs = fmap mkUnionMember . (unionRef <>)
mkUnionType OutputType ResRep {unionRef, unionCons} =
  DataUnion . map mkUnionMember . (unionRef <>) <$> buildUnions unionCons

wrapFields :: [TypeName] -> KindedType kind a -> FieldsDefinition kind CONST -> TypeContent TRUE kind CONST
wrapFields _ InputType = DataInputObject
wrapFields interfaces OutputType = DataObject interfaces

mkFieldsDefinition :: [FieldRep (Maybe (FieldContent TRUE kind CONST))] -> FieldsDefinition kind CONST
mkFieldsDefinition = unsafeFromFields . fmap fieldByRep

fieldByRep :: FieldRep (Maybe (FieldContent TRUE kind CONST)) -> FieldDefinition kind CONST
fieldByRep FieldRep {fieldSelector, fieldTypeRef, fieldValue} =
  mkField fieldValue fieldSelector fieldTypeRef

buildUnions ::
  PackObject kind =>
  [ConsRep (Maybe (FieldContent TRUE kind CONST))] ->
  SchemaT [TypeName]
buildUnions cons =
  traverse_ buildURecType cons $> fmap consName cons
  where
    buildURecType = buildUnionRecord >=> insertType

buildUnionRecord ::
  PackObject kind =>
  ConsRep (Maybe (FieldContent TRUE kind CONST)) ->
  SchemaT (TypeDefinition kind CONST)
buildUnionRecord ConsRep {consName, consFields} = mkType consName . packObject <$> fields
  where
    fields
      | null consFields = defineEnumNull $> singleton mkNullField
      | otherwise = pure $ mkFieldsDefinition consFields

__Empty :: TypeName
__Empty = "Empty"

defineEnumNull :: SchemaT ()
defineEnumNull =
  insertType
    ( mkType __Empty (mkEnumContent [__Empty]) ::
        TypeDefinition LEAF CONST
    )

mkNullField :: FieldDefinition cat s
mkNullField = mkField Nothing "_0" (mkTypeRef __Empty)

class PackObject kind where
  packObject :: FieldsDefinition kind CONST -> TypeContent TRUE kind CONST

instance PackObject OUT where
  packObject = DataObject []

instance PackObject IN where
  packObject = DataInputObject
