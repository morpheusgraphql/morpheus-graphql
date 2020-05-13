{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Client.Transform.Inputs
  ( renderNonOutputTypes,
    renderOperationArguments,
  )
where

--
-- MORPHEUS
import Control.Monad.Reader (asks)
import Data.Morpheus.Client.Transform.Core (Converter (..), getType, typeFrom)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD (..),
    DataEnumValue (..),
    DataTypeKind (..),
    FieldDefinition (..),
    FieldName,
    Operation (..),
    RAW,
    TRUE,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    getOperationName,
    removeDuplicates,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Listable (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( resolveUpdates,
  )
import Data.Semigroup ((<>))

renderArguments :: VariableDefinitions RAW -> FieldName -> Maybe TypeD
renderArguments variables argsName
  | null variables = Nothing
  | otherwise = Just rootArgumentsType
  where
    rootArgumentsType :: TypeD
    rootArgumentsType =
      TypeD
        { tName = argsName,
          tNamespace = [],
          tCons = [ConsD {cName = argsName, cFields = map fieldD (toList variables)}],
          tMeta = Nothing,
          tKind = KindInputObject
        }
      where
        fieldD :: Variable RAW -> FieldDefinition
        fieldD Variable {variableName, variableType} =
          FieldDefinition
            { fieldName = variableName,
              fieldArgs = NoArguments,
              fieldType = variableType,
              fieldMeta = Nothing
            }

renderOperationArguments :: Operation VALID -> Converter (Maybe TypeD)
renderOperationArguments Operation {operationName} = do
  variables <- asks snd
  pure $ renderArguments variables (getOperationName operationName <> "Args")

-- INPUTS
renderNonOutputTypes :: [TypeName] -> Converter [TypeD]
renderNonOutputTypes enums = do
  variables <- toList <$> asks snd
  inputTypeRequests <- resolveUpdates [] $ map (exploreInputTypeNames . typeConName . variableType) variables
  concat <$> traverse buildInputType (removeDuplicates $ inputTypeRequests <> enums)

exploreInputTypeNames :: TypeName -> [TypeName] -> Converter [TypeName]
exploreInputTypeNames name collected
  | name `elem` collected = pure collected
  | otherwise = getType name >>= scanInpType
  where
    scanInpType TypeDefinition {typeContent, typeName} = scanType typeContent
      where
        scanType (DataInputObject fields) =
          resolveUpdates
            (name : collected)
            (map toInputTypeD $ toList fields)
          where
            toInputTypeD :: FieldDefinition -> [TypeName] -> Converter [TypeName]
            toInputTypeD FieldDefinition {fieldType = TypeRef {typeConName}} =
              exploreInputTypeNames typeConName
        scanType (DataEnum _) = pure (collected <> [typeName])
        scanType _ = pure collected

buildInputType :: TypeName -> Converter [TypeD]
buildInputType name = getType name >>= generateTypes
  where
    generateTypes TypeDefinition {typeName, typeContent} = subTypes typeContent
      where
        subTypes :: TypeContent TRUE ANY -> Converter [TypeD]
        subTypes (DataInputObject inputFields) = do
          fields <- traverse toFieldD (toList inputFields)
          pure
            [ mkInputType
                typeName
                KindInputObject
                [ ConsD
                    { cName = typeName,
                      cFields = fields
                    }
                ]
            ]
        subTypes (DataEnum enumTags) =
          pure
            [ mkInputType
                typeName
                KindEnum
                (map enumOption enumTags)
            ]
        subTypes _ = pure []

mkInputType :: TypeName -> DataTypeKind -> [ConsD] -> TypeD
mkInputType tName tKind tCons =
  TypeD
    { tName,
      tNamespace = [],
      tCons,
      tKind,
      tMeta = Nothing
    }

enumOption :: DataEnumValue -> ConsD
enumOption DataEnumValue {enumName} =
  ConsD {cName = enumName, cFields = []}

toFieldD :: FieldDefinition -> Converter FieldDefinition
toFieldD field@FieldDefinition {fieldType} = do
  typeConName <- typeFrom [] <$> getType (typeConName fieldType)
  pure $ field {fieldType = fieldType {typeConName}}
