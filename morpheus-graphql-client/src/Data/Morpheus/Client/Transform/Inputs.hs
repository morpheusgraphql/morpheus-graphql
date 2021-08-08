{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Inputs
  ( renderNonOutputTypes,
    renderOperationArguments,
  )
where

import Data.Morpheus.Client.Internal.Types
  ( ClientConsD,
    ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Internal.Utils
  ( removeDuplicates,
  )
import Data.Morpheus.Client.Transform.Core
  ( Converter (..),
    UpdateT (..),
    customScalarTypes,
    getType,
    resolveUpdates,
    typeFrom,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
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
    toAny,
  )
import Relude hiding (empty)

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
                  cFields = fieldD <$> toList variables
                }
            ]
        }
      where
        fieldD :: Variable RAW -> FieldDefinition ANY VALID
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
renderOperationArguments Operation {operationName} =
  asks ((`renderArguments` (getOperationName operationName <> "Args")) . snd)

-- INPUTS
renderNonOutputTypes ::
  [TypeName] ->
  Converter [ClientTypeDefinition]
renderNonOutputTypes leafTypes = do
  variables <- asks (toList . snd)
  inputTypeRequests <- resolveUpdates [] $ fmap (UpdateT . exploreInputTypeNames . typeConName . variableType) variables
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
            (toInputTypeD <$> toList fields)
          where
            toInputTypeD :: FieldDefinition IN VALID -> UpdateT Converter [TypeName]
            toInputTypeD FieldDefinition {fieldType = TypeRef {typeConName}} =
              UpdateT (exploreInputTypeNames typeConName)
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
        subTypes :: TypeContent TRUE ANY VALID -> Converter [ClientTypeDefinition]
        subTypes (DataInputObject inputFields) = do
          fields <- traverse toClientFieldDefinition (toList inputFields)
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
                (fmap mkConsEnum enumTags)
            ]
        subTypes DataScalar {} =
          pure
            [ mkInputType
                typeName
                KindScalar
                []
            ]
        subTypes _ = pure []

mkInputType :: TypeName -> TypeKind -> [ClientConsD ANY] -> ClientTypeDefinition
mkInputType typename clientKind clientCons =
  ClientTypeDefinition
    { clientTypeName = TypeNameTH [] typename,
      clientKind,
      clientCons
    }

toClientFieldDefinition :: FieldDefinition IN VALID -> Converter (FieldDefinition IN VALID)
toClientFieldDefinition FieldDefinition {fieldType, ..} = do
  typeConName <- typeFrom [] <$> getType (typeConName fieldType)
  pure FieldDefinition {fieldType = fieldType {typeConName}, ..}
