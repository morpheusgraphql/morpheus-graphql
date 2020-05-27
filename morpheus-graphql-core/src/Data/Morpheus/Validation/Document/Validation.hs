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
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Data.Foldable (traverse_)
import Data.Functor (($>))
--
-- Morpheus
import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    PartialImplements (..),
    Place (..),
  )
import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( KeyOf (..),
    Selectable (..),
    elems,
    failure,
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
    inArgument,
    inField,
    inInterface,
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
  inInterface typeName $
    traverse_ (checkInterfaceField objFields) (elems fields)

checkInterfaceField ::
  FieldsDefinition OUT ->
  FieldDefinition OUT ->
  SchemaValidator (TypeName, TypeName) ()
checkInterfaceField
  objFields
  interfaceField@FieldDefinition
    { fieldName
    } =
    inField fieldName $
      selectOr err checkEq fieldName objFields
    where
      err = failImplements Missing
      -----
      checkEq field = interfaceField << field

class PartialImplements ctx => TypeEq a ctx where
  (<<) :: a -> a -> SchemaValidator ctx ()

instance TypeEq (FieldDefinition OUT) (TypeName, TypeName, FieldName) where
  FieldDefinition
    { fieldType,
      fieldArgs
    }
    << FieldDefinition
      { fieldType = fieldType',
        fieldArgs = fieldArgs'
      } = (fieldType << fieldType') *> (fieldArgs << fieldArgs')

instance (PartialImplements ctx) => TypeEq TypeRef ctx where
  t1@TypeRef
    { typeConName,
      typeWrappers = w1
    }
    << t2@TypeRef
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
elemIn el = selectOr (failImplements Missing) (<< el) (keyOf el)

instance TypeEq ArgumentsDefinition (TypeName, TypeName, FieldName) where
  args1 << args2 = traverse_ validateArg (elems args2)
    where
      validateArg arg = inArgument (keyOf arg) $ elemIn arg args1

instance TypeEq ArgumentDefinition (TypeName, TypeName, FieldName, FieldName) where
  arg1 << arg2 = fieldType arg1 << fieldType arg2

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
