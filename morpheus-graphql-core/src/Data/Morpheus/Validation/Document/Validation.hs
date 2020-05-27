{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
    validateSchema,
  )
where

import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Functor (($>))
--
-- Morpheus
import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    Place (..),
    partialImplements,
  )
import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( KeyOf (..),
    Selectable (..),
    elems,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition,
    ArgumentsDefinition,
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    OUT,
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    TypeWrapper,
    isWeaker,
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Context (..),
    SchemaValidator,
    constraintInterface,
    inField,
    inType,
    runSchemaValidator,
    selectType,
  )

validateSchema :: Schema -> Eventless Schema
validateSchema schema = validatePartialDocument (elems schema) $> schema

validatePartialDocument :: [TypeDefinition ANY] -> Eventless [TypeDefinition ANY]
validatePartialDocument types =
  runSchemaValidator
    (traverse validateType types)
    Context
      { types,
        local = ()
      }

validateType ::
  TypeDefinition ANY ->
  SchemaValidator () (TypeDefinition ANY)
validateType
  dt@TypeDefinition
    { typeName,
      typeContent = DataObject {objectImplements, objectFields}
    } = inType typeName $ do
    validateImplements objectImplements objectFields
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
  traverse_ (checkInterfaceField typeName objFields) (elems fields)

checkInterfaceField ::
  TypeName ->
  FieldsDefinition OUT ->
  FieldDefinition OUT ->
  SchemaValidator TypeName ()
checkInterfaceField
  typeName
  objFields
  interfaceField@FieldDefinition
    { fieldName
    } =
    inField fieldName $
      selectOr err checkEq fieldName objFields
    where
      err = [(Place typeName fieldName, Nothing, UndefinedField)]
      -----
      checkEq field = interfaceField << field

class TypeEq a ctx where
  (<<) :: a -> a -> SchemaValidator ctx ()

instance TypeEq (FieldDefinition OUT) (TypeName, FieldName) where
  FieldDefinition
    { fieldType,
      fieldArgs
    }
    << FieldDefinition
      { fieldType = fieldType',
        fieldArgs = fieldArgs'
      } = (fieldType << fieldType') <> (fieldArgs << fieldArgs')

instance TypeEq TypeRef ctx where
  t1@TypeRef
    { typeConName,
      typeWrappers = w1
    }
    << t2@TypeRef
      { typeConName = name',
        typeWrappers = w2
      }
      | typeConName == name' && not (isWeaker w2 w1) = pure ()
      | otherwise = [UnexpectedType {expectedType = t1, foundType = t2}]

elemIn ::
  ( KeyOf a,
    Selectable c a,
    TypeEq a ctx
  ) =>
  a ->
  c ->
  SchemaValidator ctx ()
elemIn el = selectOr [UndefinedField] (<< el) (keyOf el)

instance TypeEq ArgumentsDefinition (TypeName, FieldName) where
  args1 << args2 = traverse_ (`elemIn` args1) (elems args2)

instance TypeEq ArgumentDefinition (TypeName, FieldName, FieldName) where
  arg1 << arg2 = fieldType arg1 << fieldType arg2

-------------------------------
selectInterface ::
  TypeName ->
  SchemaValidator ctx (TypeName, FieldsDefinition OUT)
selectInterface = selectType >=> constraintInterface
