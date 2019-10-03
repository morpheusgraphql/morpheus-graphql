{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Parsing.JSONSchema.Parse
  ( decodeIntrospection
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy                    (ByteString)
import           Data.Morpheus.Error.Internal            (internalError)
import           Data.Morpheus.Parsing.Internal.Create   (createArgument, createDataTypeLib, createEnumType,
                                                          createField, createScalarType, createType, createUnionType)
import           Data.Morpheus.Parsing.JSONSchema.Types  (EnumValue (..), Field (..), InputValue (..),
                                                          Introspection (..), Schema (..), Type (..))
import           Data.Morpheus.Schema.TypeKind           (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), DataTypeLib, DataTypeWrapper (..),
                                                          WrapperD, toHSWrappers)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.IO                  (JSONResponse (..))
import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text, pack)

typeFromJSON :: Type -> Validation (Text, DataFullType)
typeFromJSON Type {name = Just typeName, kind = SCALAR} = pure $ createScalarType typeName
typeFromJSON Type {name = Just typeName, kind = ENUM, enumValues = Just enums} =
  pure $ createEnumType typeName (map enumName enums)
typeFromJSON Type {name = Just typeName, kind = UNION, possibleTypes = Just unions} =
  case traverse name unions of
    Nothing  -> fail "ERROR: GQL ERROR"
    Just uni -> pure $ createUnionType typeName uni
typeFromJSON Type {name = Just typeName, kind = INPUT_OBJECT, inputFields = Just iFields} = do
  fields <- traverse iField iFields
  pure (typeName, InputObject $ createType typeName fields)
  where
    iField InputValue {inputName, inputType} = do
      fieldType <- fieldTypeFromJSON inputType
      pure (inputName, createField [] inputName fieldType)
typeFromJSON Type {name = Just typeName, kind = OBJECT, fields = Just oFields} = do
  fields <- traverse oField oFields
  pure (typeName, OutputObject $ createType typeName fields)
  where
    oField Field {fieldName, fieldArgs, fieldType} = do
      fType <- fieldTypeFromJSON fieldType
      args <- traverse genArg fieldArgs
      pure (fieldName, createField args fieldName fType)
      where
        genArg InputValue {inputName = argName, inputType = argType} =
          createArgument argName <$> fieldTypeFromJSON argType
typeFromJSON x = internalError $ "Unsuported type" <> pack (show x)

fieldTypeFromJSON :: Type -> Validation ([WrapperD], Text)
fieldTypeFromJSON = fmap toHs . fieldTypeRec []
  where
    toHs (w, t) = (toHSWrappers w, t)
    fieldTypeRec :: [DataTypeWrapper] -> Type -> Validation ([DataTypeWrapper], Text)
    fieldTypeRec acc Type {kind = LIST, ofType = Just ofType} = fieldTypeRec (ListType : acc) ofType
    fieldTypeRec acc Type {kind = NON_NULL, ofType = Just ofType} = fieldTypeRec (NonNullType : acc) ofType
    fieldTypeRec acc Type {name = Just name} = pure (acc, name)
    fieldTypeRec _ x = internalError $ "Unsuported Field" <> pack (show x)

schemaFromJSON :: [Type] -> Validation [(Text, DataFullType)]
schemaFromJSON = traverse typeFromJSON

decodeIntrospection :: ByteString -> Validation DataTypeLib
decodeIntrospection jsonDoc =
  case jsonSchema of
    Left errors -> internalError $ pack errors
    Right JSONResponse {responseData = Just Introspection {__schema = Schema {types}}} ->
      schemaFromJSON types >>= createDataTypeLib
    Right res -> fail $ show res
  where
    jsonSchema :: Either String (JSONResponse Introspection)
    jsonSchema = eitherDecode jsonDoc
