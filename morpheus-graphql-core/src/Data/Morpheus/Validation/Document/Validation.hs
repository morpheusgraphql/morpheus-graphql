{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
    validateSchema,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.Functor (($>))
--
-- Morpheus

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
    isWeaker,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Types.Internal.Validation
  ( InputSource (..),
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
import Data.Morpheus.Validation.Internal.Value (validateInput)

validateSchema :: Schema -> Eventless Schema
validateSchema schema = validatePartialDocument (elems schema) $> schema

validatePartialDocument :: [TypeDefinition ANY] -> Eventless [TypeDefinition ANY]
validatePartialDocument types =
  runValidator
    (traverse validateType types)
    TypeSystemContext
      { types = systemTypes <> types,
        local = ()
      }

validateType ::
  TypeDefinition ANY ->
  SchemaValidator () (TypeDefinition ANY)
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
  FieldsDefinition OUT ->
  SchemaValidator TypeName ()
validateImplements objectImplements objectFields = do
  interface <- traverse selectInterface objectImplements
  traverse_ (mustBeSubset objectFields) interface

mustBeSubset ::
  FieldsDefinition OUT -> (TypeName, FieldsDefinition OUT) -> SchemaValidator TypeName ()
mustBeSubset objFields (typeName, fields) =
  inInterface typeName $
    traverse_ (checkInterfaceField objFields) (elems fields)

checkInterfaceField ::
  FieldsDefinition OUT ->
  FieldDefinition OUT ->
  SchemaValidator Interface ()
checkInterfaceField
  objFields
  interfaceField@FieldDefinition
    { fieldName
    } =
    inField fieldName $
      selectOr err (isSuptype interfaceField) fieldName objFields
    where
      err = failImplements Missing

class PartialImplements ctx => TypeEq a ctx where
  isSuptype :: a -> a -> SchemaValidator ctx ()

instance TypeEq (FieldDefinition OUT) (Interface, FieldName) where
  FieldDefinition
    { fieldType,
      fieldContent = args1
    }
    `isSuptype` FieldDefinition
      { fieldType = fieldType',
        fieldContent = args2
      } = (fieldType `isSuptype` fieldType') *> (args1 `isSuptype` args2)

instance TypeEq (Maybe (FieldContent TRUE OUT)) (Interface, FieldName) where
  f1 `isSuptype` f2 = toARgs f1 `isSuptype` toARgs f2
    where
      toARgs :: Maybe (FieldContent TRUE OUT) -> ArgumentsDefinition
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

instance TypeEq ArgumentsDefinition (Interface, FieldName) where
  args1 `isSuptype` args2 = traverse_ validateArg (elems args1)
    where
      validateArg arg = inArgument (keyOf arg) $ elemIn arg args2

instance TypeEq ArgumentDefinition (Interface, Field) where
  arg1 `isSuptype` arg2 = fieldType arg1 `isSuptype` fieldType arg2

-------------------------------
selectInterface ::
  TypeName ->
  SchemaValidator ctx (TypeName, FieldsDefinition OUT)
selectInterface = selectType >=> constraintInterface

failImplements ::
  PartialImplements ctx =>
  ImplementsError ->
  SchemaValidator ctx a
failImplements err = do
  x <- asks local
  failure $ partialImplements x err

checkFieldArgsuments ::
  FieldDefinition OUT ->
  SchemaValidator TypeName ()
checkFieldArgsuments FieldDefinition {fieldContent = Nothing} = pure ()
checkFieldArgsuments FieldDefinition {fieldContent = Just (FieldArgs args), fieldName} = do
  typeName <- asks local
  traverse_ (validateArgumentDefaultValue typeName fieldName) (elems args)

validateArgumentDefaultValue ::
  TypeName ->
  FieldName ->
  ArgumentDefinition ->
  SchemaValidator TypeName ()
validateArgumentDefaultValue _ _ FieldDefinition {fieldContent = Nothing} = pure ()
validateArgumentDefaultValue
  typeName
  fName
  inputField@FieldDefinition {fieldName = argName} =
    startInput (SourceInputField typeName fName (Just argName)) $
      validateDefaultValue inputField

-- DEFAULT VALUE
-- TODO: implement default value validation
validateFieldDefaultValue ::
  FieldDefinition IN ->
  SchemaValidator TypeName ()
validateFieldDefaultValue inputField@FieldDefinition {fieldName} = do
  typeName <- asks local
  startInput (SourceInputField typeName fieldName Nothing) $
    validateDefaultValue inputField

validateDefaultValue ::
  FieldDefinition IN ->
  InputValidator (TypeSystemContext TypeName) ()
validateDefaultValue FieldDefinition {fieldContent = Nothing} = pure ()
validateDefaultValue
  inputField@FieldDefinition
    { fieldName,
      fieldType = TypeRef {typeWrappers},
      fieldContent = Just DefaultInputValue {defaultInputValue}
    } = do
    datatype <- askInputFieldType inputField
    _ <- validateInput typeWrappers datatype (ObjectEntry fieldName defaultInputValue)
    pure ()
