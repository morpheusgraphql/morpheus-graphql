{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Client.Transform.Inputs
  ( renderNonOutputTypes,
    leafType,
    renderOperationArguments,
  )
where

--
-- MORPHEUS
import Control.Monad.Reader (asks)
import Data.Morpheus.Client.Transform.Core (Converter (..), compileError, getType)
import Data.Morpheus.Internal.Utils
  ( nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ClientType (..),
    ConsD (..),
    DataEnumValue (..),
    DataTypeKind (..),
    FieldDefinition (..),
    Key,
    Name,
    Operation (..),
    RAW,
    TRUE,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeRef (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    getOperationName,
    removeDuplicates,
    typeFromScalar,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Failure (..),
    Listable (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( resolveUpdates,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
  )

renderArguments :: VariableDefinitions RAW -> Text -> Maybe TypeD
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
          tMeta = Nothing
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
renderNonOutputTypes :: [Key] -> Converter [ClientType]
renderNonOutputTypes enums = do
  variables <- toList <$> asks snd
  inputTypeRequests <- resolveUpdates [] $ map (exploreInputTypeNames . typeConName . variableType) variables
  concat <$> traverse buildInputType (removeDuplicates $ inputTypeRequests <> enums)

exploreInputTypeNames :: Key -> [Key] -> Converter [Key]
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
            toInputTypeD :: FieldDefinition -> [Key] -> Converter [Key]
            toInputTypeD FieldDefinition {fieldType = TypeRef {typeConName}} =
              exploreInputTypeNames typeConName
        scanType (DataEnum _) = pure (collected <> [typeName])
        scanType _ = pure collected

buildInputType :: Text -> Converter [ClientType]
buildInputType name = getType name >>= generateTypes
  where
    generateTypes TypeDefinition {typeName, typeContent} = subTypes typeContent
      where
        subTypes :: TypeContent TRUE ANY -> Converter [ClientType]
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

mkInputType :: Name -> DataTypeKind -> [ConsD] -> ClientType
mkInputType tName clientKind tCons =
  ClientType
    { clientType =
        TypeD
          { tName,
            tNamespace = [],
            tCons,
            tMeta = Nothing
          },
      clientKind
    }

enumOption :: DataEnumValue -> ConsD
enumOption DataEnumValue {enumName} =
  ConsD {cName = enumName, cFields = []}

toFieldD :: FieldDefinition -> Converter FieldDefinition
toFieldD field@FieldDefinition {fieldType} = do
  typeConName <- typeFrom [] <$> getType (typeConName fieldType)
  pure $ field {fieldType = fieldType {typeConName}}

leafType :: TypeDefinition a -> Converter ([ClientType], [Text])
leafType TypeDefinition {typeName, typeContent} = fromKind typeContent
  where
    fromKind :: TypeContent TRUE a -> Converter ([ClientType], [Text])
    fromKind DataEnum {} = pure ([], [typeName])
    fromKind DataScalar {} = pure ([], [])
    fromKind _ = failure $ compileError "Invalid schema Expected scalar"

typeFrom :: [Name] -> TypeDefinition a -> Name
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataScalar {} = typeFromScalar typeName
    __typeFrom DataObject {} = nameSpaceType path typeName
    __typeFrom DataUnion {} = nameSpaceType path typeName
    __typeFrom _ = typeName
