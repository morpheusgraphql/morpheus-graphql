{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Parsing.JSONSchema.Parse
  ( decodeIntrospection,
  )
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Error.Internal (internalError)
import Data.Morpheus.Internal.Utils
  ( Listable (..),
  )
import Data.Morpheus.Parsing.JSONSchema.Types
  ( EnumValue (..),
    Field (..),
    InputValue (..),
    Introspection (..),
    Schema (..),
    Type (..),
  )
import Data.Morpheus.Schema.TypeKind (TypeKind (..))
import Data.Morpheus.Types.IO (JSONResponse (..))
import qualified Data.Morpheus.Types.Internal.AST as AST
  ( Schema,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    DataTypeWrapper (..),
    FieldDefinition,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeWrapper,
    createArgument,
    createEnumType,
    createField,
    createScalarType,
    createType,
    createUnionType,
    msg,
    toAny,
    toHSWrappers,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup ((<>))

decodeIntrospection :: ByteString -> Eventless AST.Schema
decodeIntrospection jsonDoc = case jsonSchema of
  Left errors -> internalError $ msg errors
  Right JSONResponse {responseData = Just Introspection {__schema = Schema {types}}} ->
    traverse parse types >>= fromList . concat
  Right res -> internalError (msg $ show res)
  where
    jsonSchema :: Either String (JSONResponse Introspection)
    jsonSchema = eitherDecode jsonDoc

class ParseJSONSchema a b where
  parse :: a -> Eventless b

instance ParseJSONSchema Type [TypeDefinition ANY] where
  parse Type {name = Just typeName, kind = SCALAR} =
    pure [createScalarType typeName]
  parse Type {name = Just typeName, kind = ENUM, enumValues = Just enums} =
    pure [createEnumType typeName (map enumName enums)]
  parse Type {name = Just typeName, kind = UNION, possibleTypes = Just unions} =
    case traverse name unions of
      Nothing -> internalError "ERROR: GQL ERROR"
      Just uni -> pure [toAny $ createUnionType typeName uni]
  parse Type {name = Just typeName, kind = INPUT_OBJECT, inputFields = Just iFields} =
    do
      (fields :: [FieldDefinition]) <- traverse parse iFields
      fs <- fromList fields
      pure [createType typeName $ DataInputObject fs]
  parse Type {name = Just typeName, kind = OBJECT, fields = Just oFields} =
    do
      (fields :: [FieldDefinition]) <- traverse parse oFields
      fs <- fromList fields
      pure [createType typeName $ DataObject [] fs]
  parse _ = pure []

instance ParseJSONSchema Field FieldDefinition where
  parse Field {fieldName, fieldArgs, fieldType} = do
    fType <- fieldTypeFromJSON fieldType
    args <- traverse genArg fieldArgs >>= fromList
    pure $ createField (ArgumentsDefinition Nothing args) fieldName fType
    where
      genArg InputValue {inputName = argName, inputType = argType} =
        createArgument argName <$> fieldTypeFromJSON argType

instance ParseJSONSchema InputValue FieldDefinition where
  parse InputValue {inputName, inputType} = createField NoArguments inputName <$> fieldTypeFromJSON inputType

fieldTypeFromJSON :: Type -> Eventless ([TypeWrapper], TypeName)
fieldTypeFromJSON = fmap toHs . fieldTypeRec []
  where
    toHs (w, t) = (toHSWrappers w, t)
    fieldTypeRec ::
      [DataTypeWrapper] -> Type -> Eventless ([DataTypeWrapper], TypeName)
    fieldTypeRec acc Type {kind = LIST, ofType = Just ofType} =
      fieldTypeRec (ListType : acc) ofType
    fieldTypeRec acc Type {kind = NON_NULL, ofType = Just ofType} =
      fieldTypeRec (NonNullType : acc) ofType
    fieldTypeRec acc Type {name = Just name} = pure (acc, name)
    fieldTypeRec _ x = internalError $ "Unsuported Field" <> msg (show x)
