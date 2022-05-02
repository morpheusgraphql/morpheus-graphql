{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Global
  ( toArgumentsType,
    toGlobalDefinitions,
  )
where

import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (..),
    ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DataEnumValue (DataEnumValue, enumName),
    FieldDefinition (..),
    RAW,
    Schema (Schema, types),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    VALID,
    Variable (..),
    VariableDefinitions,
    isNotSystemTypeName,
    isResolverType,
    toAny,
  )
import Relude hiding (empty)

toArgumentsType ::
  TypeName ->
  VariableDefinitions RAW ->
  Maybe ClientTypeDefinition
toArgumentsType cName variables
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

toGlobalDefinitions :: (TypeName -> Bool) -> Schema VALID -> [ClientTypeDefinition]
toGlobalDefinitions f Schema {types} =
  mapMaybe generateGlobalType $
    filter shouldInclude (toList types)
  where
    shouldInclude t =
      not (isResolverType t)
        && isNotSystemTypeName (typeName t)
        && f (typeName t)

generateGlobalType :: TypeDefinition ANY VALID -> Maybe ClientTypeDefinition
generateGlobalType TypeDefinition {typeName, typeContent} = do
  (clientKind, clientCons) <- genContent typeContent
  pure
    ClientTypeDefinition
      { clientTypeName = TypeNameTH [] typeName,
        clientKind,
        clientCons
      }
  where
    genContent :: TypeContent TRUE ANY VALID -> Maybe (TypeKind, [ClientConstructorDefinition])
    genContent (DataInputObject inputFields) = do
      pure
        ( KindInputObject,
          [ ClientConstructorDefinition
              { cName = typeName,
                cFields = toAny <$> toList inputFields
              }
          ]
        )
    genContent (DataEnum enumTags) = pure (KindEnum, mkConsEnum <$> enumTags)
    genContent DataScalar {} = pure (KindScalar, [])
    genContent _ = Nothing

mkConsEnum :: DataEnumValue s -> ClientConstructorDefinition
mkConsEnum DataEnumValue {enumName} = ClientConstructorDefinition enumName []
