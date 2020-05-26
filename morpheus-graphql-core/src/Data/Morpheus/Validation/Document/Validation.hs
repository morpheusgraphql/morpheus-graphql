{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
    validateSchema,
  )
where

import Data.Functor (($>))
--
-- Morpheus

import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    partialImplements,
    unknownInterface,
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
    Failure (..),
  )

type CTX = [TypeDefinition ANY]

validateSchema :: Schema -> Eventless Schema
validateSchema schema = validatePartialDocument (elems schema) $> schema

validatePartialDocument :: [TypeDefinition ANY] -> Eventless [TypeDefinition ANY]
validatePartialDocument schema = traverse (validateType schema) schema

validateType ::
  CTX ->
  TypeDefinition ANY ->
  Eventless (TypeDefinition ANY)
validateType
  ctx
  dt@TypeDefinition
    { typeName,
      typeContent = DataObject {objectImplements, objectFields}
    } = do
    validateImplements ctx typeName objectImplements objectFields
    pure dt
validateType _ x = pure x

-- INETRFACE
----------------------------
validateImplements ::
  CTX ->
  TypeName ->
  [TypeName] ->
  FieldsDefinition OUT ->
  Eventless ()
validateImplements ctx typeName objectImplements objectFields = do
  interface <- traverse (getInterfaceByKey ctx) objectImplements
  case concatMap (mustBeSubset objectFields) interface of
    [] -> pure ()
    errors -> failure $ partialImplements typeName errors

mustBeSubset ::
  FieldsDefinition OUT -> (TypeName, FieldsDefinition OUT) -> [(TypeName, FieldName, ImplementsError)]
mustBeSubset objFields (typeName, fields) = concatMap (checkInterfaceField typeName objFields) (elems fields)

checkInterfaceField ::
  TypeName ->
  FieldsDefinition OUT ->
  FieldDefinition OUT ->
  [(TypeName, FieldName, ImplementsError)]
checkInterfaceField
  typeName
  objFields
  interfaceField@FieldDefinition
    { fieldName
    } =
    selectOr err checkEq fieldName objFields
    where
      err = [(typeName, fieldName, UndefinedField)]
      -----
      checkEq field = map (typeName,fieldName,) (interfaceField << field)

class TypeEq a e where
  (<<) :: a -> a -> e

instance TypeEq (FieldDefinition OUT) [ImplementsError] where
  FieldDefinition
    { fieldType,
      fieldArgs
    }
    << FieldDefinition
      { fieldType = fieldType',
        fieldArgs = fieldArgs'
      } = (fieldType << fieldType') <> (fieldArgs << fieldArgs')

instance TypeEq TypeRef [ImplementsError] where
  t1@TypeRef
    { typeConName,
      typeWrappers
    }
    << t2@TypeRef
      { typeConName = name',
        typeWrappers = typeWrappers'
      }
      | typeConName == name' && typeWrappers << typeWrappers' = []
      | otherwise = [UnexpectedType {expectedType = t1, foundType = t2}]

instance TypeEq [TypeWrapper] Bool where
  w1 << w2 = not $ isWeaker w2 w1

elemIn ::
  ( KeyOf a,
    Selectable c a,
    TypeEq a [ImplementsError]
  ) =>
  a ->
  c ->
  [ImplementsError]
elemIn el = selectOr [UndefinedField] (<< el) (keyOf el)

instance TypeEq ArgumentsDefinition [ImplementsError] where
  args1 << args2 = concatMap (`elemIn` args1) (elems args2)

instance TypeEq ArgumentDefinition [ImplementsError] where
  arg1 << arg2 = fieldType arg1 << fieldType arg2

-------------------------------
getInterfaceByKey :: CTX -> TypeName -> Eventless (TypeName, FieldsDefinition OUT)
getInterfaceByKey schema interfaceName =
  selectBy err interfaceName schema
    >>= constraintInterface
  where
    err = unknownInterface interfaceName
    constraintInterface TypeDefinition {typeContent = DataInterface {interfaceFields}} = pure (interfaceName, interfaceFields)
    constraintInterface _ = failure $ globalErrorMessage $ "type " <> msg interfaceName <> " must be an interface"
