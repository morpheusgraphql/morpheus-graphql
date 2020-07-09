{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
    ValidateSchema (..),
  )
where

import Control.Applicative ((*>), (<*>), Applicative (..), pure)
import Control.Monad ((>=>))
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.Functor (($>), (<$>), fmap)
import Data.Maybe (Maybe (..), maybe)
import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    PartialImplements (..),
  )
import Data.Morpheus.Internal.Utils
  ( KeyOf (..),
    Selectable (..),
    elems,
    empty,
    failure,
  )
import Data.Morpheus.Schema.Schema (systemTypes)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition,
    ArgumentsDefinition (..),
    CONST,
    DataEnumValue (..),
    Directive,
    DirectiveLocation (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    IN,
    OUT,
    ObjectEntry (..),
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    UnionMember (..),
    VALID,
    Value,
    isWeaker,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Types.Internal.Validation
  ( GetWith,
    InputContext,
    InputSource (..),
    InputValidator,
    Scope (..),
    ScopeKind (..),
    askInputFieldTypeByName,
    runValidator,
    startInput,
  )
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    Interface (..),
    SchemaValidator,
    TypeSystemContext (..),
    constraintInterface,
    inArgument,
    inField,
    inInterface,
    inType,
    selectType,
  )
import Data.Morpheus.Validation.Internal.Directive
  ( Validate,
    validateDirectives,
  )
import Data.Morpheus.Validation.Internal.Value
  ( Validate (..),
    ValueContext (..),
  )
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (&&),
    (.),
    (==),
    not,
    otherwise,
  )

class ValidateSchema s where
  validateSchema :: Schema s -> Eventless (Schema VALID)

instance ValidateSchema CONST where
  validateSchema
    schema@Schema
      { types,
        query,
        mutation,
        subscription
      } =
      runValidator
        __validateSchema
        Scope
          { position = Nothing,
            typename = "TODO: typename",
            kind = TYPE,
            fieldname = "TODO: fieldname"
          }
        TypeSystemContext
          { types = elems schema,
            local = ()
          }
      where
        __validateSchema :: SchemaValidator () (Schema VALID)
        __validateSchema =
          Schema
            <$> traverse validateType types
            <*> validateType query
            <*> validateOptional validateType mutation
            <*> validateOptional validateType subscription

validateOptional :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
validateOptional f = maybe (pure Nothing) (fmap Just . f)

instance ValidateSchema VALID where
  validateSchema = pure

validatePartialDocument :: [TypeDefinition ANY CONST] -> Eventless [TypeDefinition ANY CONST]
validatePartialDocument types =
  runValidator
    (traverse validateType types)
    Scope {}
    TypeSystemContext
      { types = systemTypes <> types,
        local = ()
      }
    $> types

validateType ::
  TypeDefinition cat CONST ->
  SchemaValidator () (TypeDefinition cat VALID)
validateType
  TypeDefinition
    { typeContent = content,
      typeDirectives = directives,
      ..
    } = inType typeName $ do
    typeContent <- validateTypeContent content
    typeDirectives <- validateDirectives (typeDirectiveLocation content) directives
    pure $
      TypeDefinition
        { typeContent,
          typeDirectives,
          ..
        }

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
  SchemaValidator TypeName (TypeContent TRUE cat VALID)
validateTypeContent
  DataObject
    { objectImplements,
      objectFields
    } =
    do
      validateImplements objectImplements objectFields
      DataObject objectImplements <$> traverse validateField objectFields
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
  DataEnumValue CONST -> SchemaValidator TypeName (DataEnumValue VALID)
validateEnumMember DataEnumValue {enumDirectives = directives, ..} = do
  enumDirectives <- validateDirectives ENUM_VALUE directives
  pure DataEnumValue {..}

validateUnionMember ::
  UnionMember cat CONST -> SchemaValidator TypeName (UnionMember cat VALID)
validateUnionMember UnionMember {..} = pure UnionMember {..}

validateField ::
  FieldDefinition cat CONST ->
  SchemaValidator TypeName (FieldDefinition cat VALID)
validateField
  field@FieldDefinition
    { fieldContent = content,
      fieldDirectives = directives,
      ..
    } = inField fieldName $ do
    fieldContent <- validateOptional (checkFieldContent field) content
    fieldDirectives <- validateDirectives FIELD_DEFINITION directives
    pure $
      FieldDefinition
        { fieldDirectives,
          fieldContent,
          ..
        }

checkFieldContent ::
  ValueConstraints CONST =>
  FieldDefinition cat CONST ->
  FieldContent TRUE cat CONST ->
  SchemaValidator (TypeName, FieldName) (FieldContent TRUE cat VALID)
checkFieldContent _ (FieldArgs (ArgumentsDefinition meta args)) =
  FieldArgs . ArgumentsDefinition meta
    <$> traverse
      validateArgument
      args
checkFieldContent FieldDefinition {fieldName, fieldType} (DefaultInputValue value) = do
  (typeName, fName) <- asks local
  DefaultInputValue
    <$> startInput
      (SourceInputField typeName fName Nothing)
      (validateDefaultValue fieldName fieldType value)

validateArgument ::
  ValueConstraints s =>
  ArgumentDefinition s ->
  SchemaValidator (TypeName, FieldName) (ArgumentDefinition VALID)
validateArgument
  FieldDefinition
    { fieldContent = content,
      fieldDirectives = directives,
      ..
    } =
    do
      fieldDirectives <- validateDirectives ARGUMENT_DEFINITION directives
      fieldContent <- validateOptional (validateArgumentDefaultValue fieldName fieldType) content
      pure FieldDefinition {..}

validateArgumentDefaultValue ::
  ValueConstraints s =>
  FieldName ->
  TypeRef ->
  FieldContent TRUE IN s ->
  SchemaValidator (TypeName, FieldName) (FieldContent TRUE IN VALID)
validateArgumentDefaultValue argName fieldType (DefaultInputValue value) =
  do
    (typeName, fName) <- asks local
    v <-
      startInput
        (SourceInputField typeName fName (Just argName))
        (validateDefaultValue argName fieldType value)
    pure (DefaultInputValue v)

-- INETRFACE
----------------------------
validateImplements ::
  [TypeName] ->
  FieldsDefinition OUT CONST ->
  SchemaValidator TypeName ()
validateImplements objectImplements objectFields = do
  interface <- traverse selectInterface objectImplements
  traverse_ (mustBeSubset objectFields) interface

mustBeSubset ::
  FieldsDefinition OUT CONST ->
  (TypeName, FieldsDefinition OUT CONST) ->
  SchemaValidator TypeName ()
mustBeSubset objFields (typeName, fields) =
  inInterface typeName $
    traverse_ (checkInterfaceField objFields) (elems fields)

checkInterfaceField ::
  ( ValueConstraints s,
    Validate DirectiveLocation Directive s (TypeSystemContext (Interface, FieldName)),
    TypeEq (FieldDefinition OUT s) (Interface, FieldName)
  ) =>
  FieldsDefinition OUT s ->
  FieldDefinition OUT s ->
  SchemaValidator Interface ()
checkInterfaceField
  objFields
  interfaceField@FieldDefinition
    { fieldName,
      fieldDirectives
    } =
    inField fieldName $
      do
        _ <- validateDirectives FIELD_DEFINITION fieldDirectives
        selectOr err (isSuptype interfaceField) fieldName objFields
    where
      err = failImplements Missing

class PartialImplements ctx => TypeEq a ctx where
  isSuptype :: a -> a -> SchemaValidator ctx ()

instance TypeEq (FieldDefinition OUT CONST) (Interface, FieldName) where
  FieldDefinition
    { fieldType,
      fieldContent = args1
    }
    `isSuptype` FieldDefinition
      { fieldType = fieldType',
        fieldContent = args2
      } = (fieldType `isSuptype` fieldType') *> (args1 `isSuptype` args2)

instance TypeEq (Maybe (FieldContent TRUE OUT s)) (Interface, FieldName) where
  f1 `isSuptype` f2 = toARgs f1 `isSuptype` toARgs f2
    where
      toARgs :: Maybe (FieldContent TRUE OUT s) -> ArgumentsDefinition s
      toARgs (Just (FieldArgs args)) = args
      toARgs _ = empty

instance (PartialImplements ctx) => TypeEq TypeRef ctx where
  t1@TypeRef
    { typeConName,
      typeWrappers = w1
    }
    `isSuptype` t2@TypeRef
      { typeConName = name',
        typeWrappers = w2
      }
      | typeConName == name' && not (isWeaker w2 w1) = pure ()
      | otherwise =
        failImplements UnexpectedType {expectedType = t1, foundType = t2}

elemIn ::
  ( KeyOf a,
    Selectable c a,
    TypeEq a ctx
  ) =>
  a ->
  c ->
  SchemaValidator ctx ()
elemIn el = selectOr (failImplements Missing) (isSuptype el) (keyOf el)

instance TypeEq (ArgumentsDefinition s) (Interface, FieldName) where
  args1 `isSuptype` args2 = traverse_ validateArg (elems args1)
    where
      validateArg arg = inArgument (keyOf arg) $ elemIn arg args2

instance TypeEq (ArgumentDefinition s) (Interface, Field) where
  arg1 `isSuptype` arg2 = fieldType arg1 `isSuptype` fieldType arg2

-------------------------------
selectInterface ::
  TypeName ->
  SchemaValidator ctx (TypeName, FieldsDefinition OUT CONST)
selectInterface = selectType >=> constraintInterface

failImplements ::
  PartialImplements ctx =>
  ImplementsError ->
  SchemaValidator ctx a
failImplements err = do
  x <- asks local
  failure $ partialImplements x err

-- DEFAULT VALUE

type ValueConstraints s =
  ( GetWith (TypeSystemContext (TypeName, FieldName)) (Schema s),
    Validate (ValueContext s) ObjectEntry s (InputContext (TypeSystemContext (TypeName, FieldName))),
    Validate DirectiveLocation Directive s (TypeSystemContext (TypeName, FieldName))
  )

validateDefaultValue ::
  forall s.
  ValueConstraints s =>
  FieldName ->
  TypeRef ->
  Value s ->
  InputValidator
    (TypeSystemContext (TypeName, FieldName))
    (Value VALID)
validateDefaultValue
  fieldName
  TypeRef {typeWrappers, typeConName}
  defaultInputValue =
    do
      (datatype :: TypeDefinition IN s) <- askInputFieldTypeByName typeConName
      entryValue
        <$> validate
          (ValueContext typeWrappers datatype)
          (ObjectEntry fieldName defaultInputValue)
