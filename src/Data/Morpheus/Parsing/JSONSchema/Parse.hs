{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Parsing.JSONSchema.Parse
  ( JSONType
  , Field
  , InputValue
  , typeFromJSON
  ) where

import           Data.Morpheus.Parsing.Internal.Create   (createArgument, createEnumType, createField, createScalarType,
                                                          createType, createUnionType)
import qualified Data.Morpheus.Schema.EnumValue          as E (EnumValue (..))
import qualified Data.Morpheus.Schema.Field              as F (Field (..))
import qualified Data.Morpheus.Schema.InputValue         as I (InputValue (..))
import           Data.Morpheus.Schema.JSONType           (JSONType (..))
import           Data.Morpheus.Schema.TypeKind           (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Text                               (Text)

type InputValue = I.InputValue JSONType

type Field = F.Field JSONType

typeFromJSON :: JSONType -> Validation (Text, DataFullType)
typeFromJSON JSONType {name = Just typeName, kind = SCALAR} = pure $ createScalarType typeName
typeFromJSON JSONType {name = Just typeName, kind = ENUM, enumValues = Just enums} =
  pure $ createEnumType typeName (map E.name enums)
typeFromJSON JSONType {name = Just typeName, kind = UNION, possibleTypes = Just unions} =
  case traverse name unions of
    Nothing  -> fail "ERROR: GQL ERROR"
    Just uni -> pure $ createUnionType typeName uni
typeFromJSON JSONType {name = Just typeName, kind = INPUT_OBJECT, inputFields = Just iFields} =
  pure (typeName, InputObject $ createType typeName (map iField iFields))
  where
    iField I.InputValue {I.name = fieldName, I.type' = JSONType {name = Just fieldType}} =
      (fieldName, createField () fieldName ([], fieldType))
typeFromJSON JSONType {name = Just typeName, kind = OBJECT, fields = Just oFields} =
  pure (typeName, OutputObject $ createType typeName (map oField oFields))
  where
    oField F.Field {F.name = fieldName, F.args = fArgs, F.type' = JSONType {name = Just fieldType}} =
      (fieldName, createField (map genArg fArgs) fieldName ([], fieldType))
      where
        genArg I.InputValue {I.name = argName, I.type' = JSONType {name = Just argType}} =
          createArgument argName [] argType
