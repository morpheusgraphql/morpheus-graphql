{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Inputs
  ( renderOperationArguments,
    toGlobalDefinitions,
  )
where

import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (..),
    ClientTypeDefinition (..),
    Mode (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Internal.Utils
  ( withMode,
  )
import Data.Morpheus.Client.Transform.Core
  ( Converter (..),
    getType,
    typeFrom,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DataEnumValue (DataEnumValue, enumName),
    FieldDefinition (..),
    IN,
    Operation (..),
    RAW,
    Schema (Schema, types),
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

renderOperationArguments ::
  Operation VALID ->
  Converter (Maybe ClientTypeDefinition)
renderOperationArguments Operation {operationName} =
  asks ((`renderArguments` (getOperationName operationName <> "Args")) . snd)

renderArguments ::
  VariableDefinitions RAW ->
  TypeName ->
  Maybe ClientTypeDefinition
renderArguments variables cName
  | null variables = Nothing
  | otherwise =
    Just
      ClientTypeDefinition
        { clientTypeName = TypeNameTH [] cName,
          clientKind = KindInputObject,
          clientCons =
            [ ClientConstructorDefinition
                { cName,
                  cFields = toFieldDefinition <$> toList variables
                }
            ]
        }

toFieldDefinition :: Variable RAW -> FieldDefinition ANY VALID
toFieldDefinition Variable {variableName, variableType} =
  FieldDefinition
    { fieldName = variableName,
      fieldContent = Nothing,
      fieldType = variableType,
      fieldDescription = Nothing,
      fieldDirectives = empty
    }

toGlobalDefinitions :: (TypeName -> Bool) -> Schema VALID -> GQLResult [ClientTypeDefinition]
toGlobalDefinitions f schema@Schema {types} =
  flip runReaderT (schema, empty) $
    runConverter
      ( catMaybes
          <$> traverse generateGlobalType (filter shouldInclude (toList types))
      )
  where
    shouldInclude t = withMode Global t && f (typeName t)

generateGlobalType :: TypeDefinition ANY VALID -> Converter (Maybe ClientTypeDefinition)
generateGlobalType TypeDefinition {typeName, typeContent} = do
  content <- subTypes typeContent
  pure $ case content of
    Nothing -> Nothing
    Just (clientKind, clientCons) ->
      pure
        ClientTypeDefinition
          { clientTypeName = TypeNameTH [] typeName,
            clientKind,
            clientCons
          }
  where
    subTypes :: TypeContent TRUE ANY VALID -> Converter (Maybe (TypeKind, [ClientConstructorDefinition]))
    subTypes (DataInputObject inputFields) = do
      fields <- traverse toClientFieldDefinition (toList inputFields)
      pure $
        Just
          ( KindInputObject,
            [ClientConstructorDefinition {cName = typeName, cFields = fmap toAny fields}]
          )
    subTypes (DataEnum enumTags) = pure $ Just (KindEnum, mkConsEnum <$> enumTags)
    subTypes DataScalar {} = pure $ Just (KindScalar, [])
    subTypes _ = pure Nothing

mkConsEnum :: DataEnumValue s -> ClientConstructorDefinition
mkConsEnum DataEnumValue {enumName} = ClientConstructorDefinition enumName []

toClientFieldDefinition :: FieldDefinition IN VALID -> Converter (FieldDefinition IN VALID)
toClientFieldDefinition FieldDefinition {fieldType, ..} = do
  typeConName <- typeFrom [] <$> getType (typeConName fieldType)
  pure FieldDefinition {fieldType = fieldType {typeConName}, ..}
