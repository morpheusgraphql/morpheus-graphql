{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Global
  ( toArgumentsType,
    toGlobalDefinitions,
  )
where

import Data.Morpheus.Client.Internal.AST
  ( ClientDeclaration,
    ClientTypeDefinition (..),
  )
import Data.Morpheus.Client.Transform.Core (toClientDeclarations, toCodeGenField)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenTypeName (..),
    fromTypeName,
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
  )
import Relude hiding (empty)

toArgumentsType ::
  CodeGenTypeName ->
  VariableDefinitions RAW ->
  Maybe ClientTypeDefinition
toArgumentsType clientTypeName variables
  | null variables = Nothing
  | otherwise =
      Just
        ClientTypeDefinition
          { clientTypeName,
            clientKind = KindInputObject,
            clientCons =
              [ CodeGenConstructor
                  { constructorName = clientTypeName,
                    constructorFields = toCodeGenField . toFieldDefinition <$> toList variables
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

toGlobalDefinitions :: (TypeName -> Bool) -> Schema VALID -> [ClientDeclaration]
toGlobalDefinitions f Schema {types} =
  concatMap toClientDeclarations $
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
      { clientTypeName = fromTypeName typeName,
        clientKind,
        clientCons
      }
  where
    genContent :: TypeContent TRUE ANY VALID -> Maybe (TypeKind, [CodeGenConstructor])
    genContent (DataInputObject inputFields) =
      pure
        ( KindInputObject,
          [ CodeGenConstructor
              { constructorName = fromTypeName typeName,
                constructorFields = toCodeGenField <$> toList inputFields
              }
          ]
        )
    genContent (DataEnum enumTags) = pure (KindEnum, mkConsEnum typeName <$> enumTags)
    genContent DataScalar {} = pure (KindScalar, [])
    genContent _ = Nothing

mkConsEnum :: TypeName -> DataEnumValue s -> CodeGenConstructor
mkConsEnum typename DataEnumValue {enumName} =
  CodeGenConstructor (CodeGenTypeName [coerce typename] [] enumName) []
