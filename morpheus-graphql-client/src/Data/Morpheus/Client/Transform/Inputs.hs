{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Transform.Core
  ( Converter (..),
    customScalarTypes,
    getType,
    typeFrom,
  )
import Data.Morpheus.Internal.Utils
  ( elems,
    empty,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldDefinition (..),
    IN,
    Operation (..),
    RAW,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    getOperationName,
    mkConsEnum,
    removeDuplicates,
    toAny,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( resolveUpdates,
  )
import Data.Semigroup ((<>))

renderArguments ::
  VariableDefinitions RAW ->
  TypeName ->
  Maybe ClientTypeDefinition
renderArguments variables cName
  | null variables = Nothing
  | otherwise = Just rootArgumentsType
  where
    rootArgumentsType :: ClientTypeDefinition
    rootArgumentsType =
      ClientTypeDefinition
        { clientTypeName = TypeNameTH [] cName,
          clientKind = KindInputObject,
          clientCons =
            [ ConsD
                { cName,
                  cFields = map fieldD (elems variables)
                }
            ]
        }
      where
        fieldD :: Variable RAW -> FieldDefinition ANY
        fieldD Variable {variableName, variableType} =
          FieldDefinition
            { fieldName = variableName,
              fieldContent = Nothing,
              fieldType = variableType,
              fieldDescription = Nothing,
              fieldDirectives = empty
            }

renderOperationArguments ::
  Operation VALID ->
  Converter (Maybe ClientTypeDefinition)
renderOperationArguments Operation {operationName} = do
  variables <- asks snd
  pure $ renderArguments variables (getOperationName operationName <> "Args")

-- INPUTS
renderNonOutputTypes ::
  [TypeName] ->
  Converter [ClientTypeDefinition]
renderNonOutputTypes leafTypes = do
  variables <- elems <$> asks snd
  inputTypeRequests <- resolveUpdates [] $ map (exploreInputTypeNames . typeConName . variableType) variables
  concat <$> traverse buildInputType (removeDuplicates $ inputTypeRequests <> leafTypes)

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
            (map toInputTypeD $ elems fields)
          where
            toInputTypeD :: FieldDefinition IN -> [TypeName] -> Converter [TypeName]
            toInputTypeD FieldDefinition {fieldType = TypeRef {typeConName}} =
              exploreInputTypeNames typeConName
        scanType (DataEnum _) = pure (collected <> [typeName])
        scanType (DataScalar _) = pure (collected <> customScalarTypes typeName)
        scanType _ = pure collected

buildInputType ::
  TypeName ->
  Converter [ClientTypeDefinition]
buildInputType name = getType name >>= generateTypes
  where
    generateTypes TypeDefinition {typeName, typeContent} = subTypes typeContent
      where
        subTypes :: TypeContent TRUE ANY -> Converter [ClientTypeDefinition]
        subTypes (DataInputObject inputFields) = do
          fields <- traverse toClientFieldDefinition (elems inputFields)
          pure
            [ mkInputType
                typeName
                KindInputObject
                [ ConsD
                    { cName = typeName,
                      cFields = fmap toAny fields
                    }
                ]
            ]
        subTypes (DataEnum enumTags) =
          pure
            [ mkInputType
                typeName
                KindEnum
                (map mkConsEnum enumTags)
            ]
        subTypes DataScalar {} =
          pure
            [ mkInputType
                typeName
                KindScalar
                []
            ]
        subTypes _ = pure []

mkInputType :: TypeName -> TypeKind -> [ConsD ANY] -> ClientTypeDefinition
mkInputType typename clientKind clientCons =
  ClientTypeDefinition
    { clientTypeName = TypeNameTH [] typename,
      clientKind,
      clientCons
    }

toClientFieldDefinition :: FieldDefinition IN -> Converter (FieldDefinition IN)
toClientFieldDefinition FieldDefinition {fieldType, ..} = do
  typeConName <- typeFrom [] <$> getType (typeConName fieldType)
  pure FieldDefinition {fieldType = fieldType {typeConName}, ..}
