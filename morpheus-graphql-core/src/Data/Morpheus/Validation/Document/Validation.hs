{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Data.Morpheus.Internal.Utils
  ( Listable (..),
    Selectable (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition (..),
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    isWeaker,
    lookupWith,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Failure (..),
  )

validateSchema :: Schema -> Eventless Schema
validateSchema schema = validatePartialDocument (toList schema) $> schema

validatePartialDocument :: [TypeDefinition ANY] -> Eventless [TypeDefinition ANY]
validatePartialDocument lib = traverse validateType lib
  where
    validateType :: TypeDefinition ANY -> Eventless (TypeDefinition ANY)
    validateType dt@TypeDefinition {typeName, typeContent = DataObject {objectImplements, objectFields}} = do
      interface <- traverse getInterfaceByKey objectImplements
      case concatMap (mustBeSubset objectFields) interface of
        [] -> pure dt
        errors -> failure $ partialImplements typeName errors
    validateType x = pure x
    mustBeSubset ::
      FieldsDefinition -> (TypeName, FieldsDefinition) -> [(TypeName, FieldName, ImplementsError)]
    mustBeSubset objFields (typeName, fields) = concatMap checkField (toList fields)
      where
        checkField :: FieldDefinition -> [(TypeName, FieldName, ImplementsError)]
        checkField FieldDefinition {fieldName, fieldType = interfaceT@TypeRef {typeConName = interfaceTypeName, typeWrappers = interfaceWrappers}} =
          selectOr err checkTypeEq fieldName objFields
          where
            err = [(typeName, fieldName, UndefinedField)]
            checkTypeEq FieldDefinition {fieldType = objT@TypeRef {typeConName, typeWrappers}}
              | typeConName == interfaceTypeName && not (isWeaker typeWrappers interfaceWrappers) =
                []
              | otherwise =
                [ ( typeName,
                    fieldName,
                    UnexpectedType
                      { expectedType = interfaceT,
                        foundType = objT
                      }
                  )
                ]
    -------------------------------
    getInterfaceByKey :: TypeName -> Eventless (TypeName, FieldsDefinition)
    getInterfaceByKey interfaceName = case lookupWith typeName interfaceName lib of
      Just TypeDefinition {typeContent = DataInterface {interfaceFields}} -> pure (interfaceName, interfaceFields)
      _ -> failure $ unknownInterface interfaceName
