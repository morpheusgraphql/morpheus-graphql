{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Document.Validation
  ( ValidateSchema (..),
  )
where

import Data.Morpheus.Error.Document.Interface
  ( Field (..),
    ImplementsError (..),
    TypeSystemElement (..),
    inArgument,
    inField,
    inInterface,
    inType,
    partialImplements,
  )
import Data.Morpheus.Ext.Result
  ( Eventless,
  )
import Data.Morpheus.Ext.SemigroupM
  ( (<:>),
  )
import Data.Morpheus.Internal.Utils
  ( KeyOf (..),
    Selectable (..),
    empty,
    failure,
  )
import Data.Morpheus.Schema.Schema
  ( internalSchema,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition (..),
    ArgumentsDefinition,
    CONST,
    DataEnumValue (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    IN,
    OUT,
    Schema (..),
    Schema (..),
    Subtyping (..),
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    Typed (..),
    UnionMember (..),
    VALID,
    Value,
    mkBaseType,
  )
import Data.Morpheus.Types.Internal.Config (Config (..))
import Data.Morpheus.Types.Internal.Validation
  ( InputSource (..),
    InputValidator,
    Scope (..),
    ScopeKind (..),
    runValidator,
    selectType,
    startInput,
  )
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( SchemaValidator,
    TypeSystemContext (..),
    constraintInterface,
  )
import Data.Morpheus.Validation.Internal.Directive
  ( validateDirectives,
  )
import Data.Morpheus.Validation.Internal.Value
  ( validateInputByTypeRef,
  )
import Relude hiding (empty, local)

class ValidateSchema s where
  validateSchema :: Bool -> Config -> Schema s -> Eventless (Schema VALID)

instance ValidateSchema CONST where
  validateSchema
    withSystem
    config
    schema@Schema
      { types,
        query,
        mutation,
        subscription,
        directiveDefinitions
      } = do
      sysSchema <-
        if withSystem
          then internalSchema <:> schema
          else pure schema
      runValidator
        __validateSchema
        config
        sysSchema
        Scope
          { position = Nothing,
            currentTypeName = "Root",
            currentTypeKind = KindObject Nothing,
            currentTypeWrappers = mkBaseType,
            kind = TYPE,
            fieldname = "Root"
          }
        TypeSystemContext
          { local = ()
          }
      where
        __validateSchema :: SchemaValidator () (Schema VALID)
        __validateSchema =
          Schema
            <$> traverse validateType types
            <*> validateType query
            <*> validateOptional validateType mutation
            <*> validateOptional validateType subscription
            <*> traverse validateDirectiveDefinition directiveDefinitions

validateOptional :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
validateOptional f = maybe (pure Nothing) (fmap Just . f)

instance ValidateSchema VALID where
  validateSchema _ _ = pure

validateType ::
  TypeDefinition cat CONST ->
  SchemaValidator () (TypeDefinition cat VALID)
validateType
  TypeDefinition
    { typeName,
      typeDescription,
      typeDirectives,
      typeContent
    } =
    inType typeName $
      TypeDefinition
        typeDescription
        typeName
        <$> validateDirectives (typeDirectiveLocation typeContent) typeDirectives
        <*> validateTypeContent typeContent

typeDirectiveLocation :: TypeContent a b c -> DirectiveLocation
typeDirectiveLocation DataObject {} = OBJECT
typeDirectiveLocation DataInputObject {} = INPUT_OBJECT
typeDirectiveLocation DataScalar {} = SCALAR
typeDirectiveLocation DataEnum {} = ENUM
typeDirectiveLocation DataInputUnion {} = OBJECT
typeDirectiveLocation DataUnion {} = UNION
typeDirectiveLocation DataInterface {} = INTERFACE

validateTypeContent ::
  TypeContent TRUE cat CONST ->
  SchemaValidator TypeSystemElement (TypeContent TRUE cat VALID)
validateTypeContent
  DataObject
    { objectImplements,
      objectFields
    } =
    DataObject
      <$> validateImplements objectImplements objectFields
      <*> traverse validateField objectFields
validateTypeContent DataInputObject {inputObjectFields} =
  DataInputObject <$> traverse validateField inputObjectFields
validateTypeContent DataScalar {..} = pure DataScalar {..}
validateTypeContent DataEnum {enumMembers} = DataEnum <$> traverse validateEnumMember enumMembers
validateTypeContent DataInputUnion {inputUnionMembers} =
  DataInputUnion <$> traverse validateUnionMember inputUnionMembers
validateTypeContent DataUnion {unionMembers} = DataUnion <$> traverse validateUnionMember unionMembers
validateTypeContent (DataInterface fields) =
  DataInterface <$> traverse validateField fields

validateEnumMember ::
  DataEnumValue CONST -> SchemaValidator TypeSystemElement (DataEnumValue VALID)
validateEnumMember DataEnumValue {enumDirectives = directives, ..} =
  DataEnumValue enumDescription enumName
    <$> validateDirectives ENUM_VALUE directives

validateUnionMember ::
  UnionMember cat CONST -> SchemaValidator TypeSystemElement (UnionMember cat VALID)
validateUnionMember UnionMember {..} = pure UnionMember {..}

class FieldDirectiveLocation (cat :: TypeCategory) where
  directiveLocation :: Proxy cat -> DirectiveLocation

instance FieldDirectiveLocation OUT where
  directiveLocation _ = FIELD_DEFINITION

instance FieldDirectiveLocation IN where
  directiveLocation _ = INPUT_FIELD_DEFINITION

validateField ::
  forall cat.
  FieldDirectiveLocation cat =>
  FieldDefinition cat CONST ->
  SchemaValidator TypeSystemElement (FieldDefinition cat VALID)
validateField field@FieldDefinition {..} =
  inField
    fieldName
    ( FieldDefinition
        fieldDescription
        fieldName
        fieldType
        <$> validateOptional (checkFieldContent field) fieldContent
        <*> validateDirectives (directiveLocation (Proxy @cat)) fieldDirectives
    )

checkFieldContent ::
  FieldDefinition cat CONST ->
  FieldContent TRUE cat CONST ->
  SchemaValidator Field (FieldContent TRUE cat VALID)
checkFieldContent _ (FieldArgs argsDef) = FieldArgs <$> validateArgumentsDefinition argsDef
checkFieldContent FieldDefinition {fieldType} (DefaultInputValue value) = do
  (typeName, fName) <- getTypeAndFieldName
  DefaultInputValue
    <$> startInput
      (SourceInputField typeName fName Nothing)
      (validateDefaultValue fieldType value)

getTypeAndFieldName :: SchemaValidator Field (TypeName, FieldName)
getTypeAndFieldName = do
  Field fName _ t <- asks local
  pure (getTypeName t, fName)
  where
    getTypeName (Type x) = x
    getTypeName (Interface _ x) = x

validateArgumentsDefinition ::
  ArgumentsDefinition CONST ->
  SchemaValidator Field (ArgumentsDefinition VALID)
validateArgumentsDefinition = traverse validateArgumentDefinition

validateArgumentDefinition ::
  ArgumentDefinition CONST ->
  SchemaValidator Field (ArgumentDefinition VALID)
validateArgumentDefinition (ArgumentDefinition FieldDefinition {..}) =
  ArgumentDefinition
    <$> ( FieldDefinition
            fieldDescription
            fieldName
            fieldType
            <$> validateOptional (validateArgumentDefaultValue fieldName fieldType) fieldContent
            <*> validateDirectives ARGUMENT_DEFINITION fieldDirectives
        )

validateArgumentDefaultValue ::
  FieldName ->
  TypeRef ->
  FieldContent TRUE IN CONST ->
  SchemaValidator Field (FieldContent TRUE IN VALID)
validateArgumentDefaultValue argName fieldType (DefaultInputValue value) =
  do
    (typeName, fName) <- getTypeAndFieldName
    v <-
      startInput
        (SourceInputField typeName fName (Just argName))
        (validateDefaultValue fieldType value)
    pure (DefaultInputValue v)

-- INTERFACE
----------------------------
validateImplements ::
  [TypeName] ->
  FieldsDefinition OUT CONST ->
  SchemaValidator TypeSystemElement [TypeName]
validateImplements objectImplements objectFields =
  ( traverse selectInterface objectImplements
      >>= traverse_ (mustBeSubset objectFields)
  )
    $> objectImplements

mustBeSubset ::
  FieldsDefinition OUT CONST ->
  (TypeName, FieldsDefinition OUT CONST) ->
  SchemaValidator TypeSystemElement ()
mustBeSubset objFields (typeName, fields) =
  inInterface typeName $
    traverse_ (checkInterfaceField objFields) fields

checkInterfaceField ::
  FieldsDefinition OUT CONST ->
  FieldDefinition OUT CONST ->
  SchemaValidator TypeSystemElement ()
checkInterfaceField
  objFields
  interfaceField@FieldDefinition
    { fieldName,
      fieldDirectives
    } =
    inField fieldName $
      validateDirectives FIELD_DEFINITION fieldDirectives
        *> selectOr err (`isCompatibleTo` interfaceField) fieldName objFields
    where
      err = failImplements Missing

class StructuralCompatibility a where
  isCompatibleTo :: a -> a -> SchemaValidator Field ()

isCompatibleBy :: StructuralCompatibility a => (t -> a) -> t -> t -> SchemaValidator Field ()
isCompatibleBy f a b = f a `isCompatibleTo` f b

instance StructuralCompatibility (FieldDefinition OUT CONST) where
  f1 `isCompatibleTo` f2 =
    isCompatibleBy fieldType f1 f2
      *> isCompatibleBy (fieldArgs . fieldContent) f1 f2

fieldArgs :: Maybe (FieldContent TRUE OUT s) -> ArgumentsDefinition s
fieldArgs (Just (FieldArgs args)) = args
fieldArgs _ = empty

instance StructuralCompatibility (ArgumentsDefinition s) where
  subArguments `isCompatibleTo` arguments = traverse_ hasCompatibleSubArgument arguments
    where
      hasCompatibleSubArgument :: ArgumentDefinition s -> SchemaValidator Field ()
      hasCompatibleSubArgument argument =
        inArgument (keyOf argument) $
          selectOr (failImplements Missing) (`isCompatibleTo` argument) (keyOf argument) subArguments

instance StructuralCompatibility (ArgumentDefinition s) where
  isCompatibleTo = isCompatibleBy (fieldType . argument)

instance StructuralCompatibility TypeRef where
  t1 `isCompatibleTo` t2
    | t1 `isSubtype` t2 = pure ()
    | otherwise = failImplements UnexpectedType {expectedType = t2, foundType = t1}

-------------------------------
selectInterface ::
  TypeName ->
  SchemaValidator ctx (TypeName, FieldsDefinition OUT CONST)
selectInterface = selectType >=> constraintInterface

failImplements ::
  ImplementsError ->
  SchemaValidator Field a
failImplements err = do
  x <- asks local
  failure $ partialImplements x err

-- DEFAULT VALUE

validateDefaultValue ::
  TypeRef ->
  Value CONST ->
  InputValidator
    CONST
    (TypeSystemContext Field)
    (Value VALID)
validateDefaultValue typeRef =
  validateInputByTypeRef (Typed typeRef)

validateDirectiveDefinition :: DirectiveDefinition CONST -> SchemaValidator () (DirectiveDefinition VALID)
validateDirectiveDefinition DirectiveDefinition {directiveDefinitionArgs = args, ..} =
  inType "Directive" $ inField directiveDefinitionName $ do
    directiveDefinitionArgs <- validateArgumentsDefinition args
    pure DirectiveDefinition {..}
