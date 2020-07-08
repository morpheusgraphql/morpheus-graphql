{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
    ValidateSchema (..),
  )
where

import Control.Applicative ((*>), pure)
import Control.Monad ((>=>))
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (Maybe (..))
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
    DirectiveLocation (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    IN,
    OUT,
    ObjectEntry (..),
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    VALID,
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
    askInputFieldType,
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
  ( ValidateDirective,
    shouldIncludeSelection,
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
    (==),
    id,
    not,
    otherwise,
    undefined,
  )

class ValidateSchema s where
  validateSchema :: Schema s -> Eventless (Schema VALID)

instance ValidateSchema CONST

--validateSchema schema = undefined -- TODO: -- validatePartialDocument (elems schema) $> schema

instance ValidateSchema VALID where
  validateSchema = pure

validatePartialDocument :: [TypeDefinition ANY CONST] -> Eventless [TypeDefinition ANY CONST]
validatePartialDocument types =
  runValidator
    (traverse validateType types)
    TypeSystemContext
      { types = systemTypes <> types,
        local = ()
      }

validateType ::
  TypeDefinition ANY CONST ->
  SchemaValidator () (TypeDefinition ANY CONST)
validateType
  dt@TypeDefinition
    { typeName,
      typeContent =
        DataObject
          { objectImplements,
            objectFields
          }
    } = inType typeName $
    do
      validateImplements objectImplements objectFields
      traverse_ checkFieldArgsuments objectFields
      pure dt
validateType
  dt@TypeDefinition
    { typeContent = DataInputObject {inputObjectFields},
      typeName
    } = inType typeName $ do
    traverse_ validateFieldDefaultValue inputObjectFields
    pure dt
validateType x = pure x

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
    ValidateDirective s (TypeSystemContext (Interface, FieldName)),
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

checkFieldArgsuments ::
  ValueConstraints s =>
  FieldDefinition OUT s ->
  SchemaValidator TypeName ()
checkFieldArgsuments FieldDefinition {fieldContent = Nothing} = pure ()
checkFieldArgsuments FieldDefinition {fieldContent = Just (FieldArgs args), fieldName} = do
  typeName <- asks local
  traverse_ (validateArgumentDefaultValue typeName fieldName) (elems args)

validateArgumentDefaultValue ::
  ValueConstraints s =>
  TypeName ->
  FieldName ->
  ArgumentDefinition s ->
  SchemaValidator TypeName ()
validateArgumentDefaultValue _ _ FieldDefinition {fieldContent = Nothing} = pure ()
validateArgumentDefaultValue
  typeName
  fName
  inputField@FieldDefinition {fieldName = argName} =
    startInput (SourceInputField typeName fName (Just argName)) $
      validateDefaultValue inputField

-- DEFAULT VALUE
validateFieldDefaultValue ::
  ValueConstraints s =>
  FieldDefinition IN s ->
  SchemaValidator TypeName ()
validateFieldDefaultValue inputField@FieldDefinition {fieldName} = do
  typeName <- asks local
  startInput (SourceInputField typeName fieldName Nothing) $
    validateDefaultValue inputField

type ValueConstraints s =
  ( GetWith (TypeSystemContext TypeName) (Schema s),
    Validate (ValueContext s) ObjectEntry s (InputContext (TypeSystemContext TypeName))
  )

validateDefaultValue ::
  ValueConstraints s =>
  FieldDefinition IN s ->
  InputValidator (TypeSystemContext TypeName) ()
validateDefaultValue FieldDefinition {fieldContent = Nothing} = pure ()
validateDefaultValue
  inputField@FieldDefinition
    { fieldName,
      fieldType = TypeRef {typeWrappers},
      fieldContent = Just DefaultInputValue {defaultInputValue}
    } = do
    datatype <- askInputFieldType inputField
    _ <-
      validate
        (ValueContext typeWrappers datatype)
        (ObjectEntry fieldName defaultInputValue)
    pure ()
