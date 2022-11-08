{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Interpreting.Global
  ( toGlobalDefinitions,
  )
where

import Data.Morpheus.Client.CodeGen.AST
  ( ClientDeclaration,
    ClientPreDeclaration (..),
    ClientTypeDefinition (..),
    DERIVING_MODE (..),
  )
import Data.Morpheus.Client.CodeGen.Interpreting.Core (printClientType)
import Data.Morpheus.Client.CodeGen.Interpreting.PreDeclarations
  ( mapPreDeclarations,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenTypeName (..),
    FIELD_TYPE_WRAPPER (..),
    fromTypeName,
  )
import Data.Morpheus.CodeGen.Utils (Flag (FlagLanguageExtension), Flags)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DataEnumValue (DataEnumValue, enumName),
    FieldDefinition (..),
    Schema (Schema, types),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    VALID,
    isNotSystemTypeName,
    isNullable,
    isResolverType,
  )
import Relude hiding (empty)

toClientDeclarations :: ClientTypeDefinition -> [ClientPreDeclaration]
toClientDeclarations def@ClientTypeDefinition {clientKind}
  | KIND_SCALAR == clientKind = [FromJSONClass SCALAR_MODE cgType, ToJSONClass SCALAR_MODE cgType]
  | KIND_ENUM == clientKind = [ClientType cgType, FromJSONClass ENUM_MODE cgType, ToJSONClass ENUM_MODE cgType]
  | otherwise = [ClientType cgType, ToJSONClass TYPE_MODE cgType]
  where
    cgType = printClientType def

toGlobalDefinitions :: (TypeName -> Bool) -> Schema VALID -> GQLResult ([ClientDeclaration], Flags)
toGlobalDefinitions f Schema {types} = do
  let tyDefs = mapMaybe generateGlobalType $ filter shouldInclude (sortWith typeName $ toList types)
  decs <- traverse mapPreDeclarations $ concatMap toClientDeclarations tyDefs
  let hasEnums = not $ null [x | x@ClientTypeDefinition {clientKind = KIND_ENUM} <- tyDefs]
  pure (decs, [FlagLanguageExtension "LambdaCase" | hasEnums])
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
        ( KIND_INPUT_OBJECT,
          [ CodeGenConstructor
              { constructorName = fromTypeName typeName,
                constructorFields = toCodeGenField <$> toList inputFields
              }
          ]
        )
    genContent (DataEnum enumTags) = pure (KIND_ENUM, mkConsEnum typeName <$> enumTags)
    genContent DataScalar {} = pure (KIND_SCALAR, [])
    genContent _ = Nothing

toCodeGenField :: FieldDefinition a b -> CodeGenField
toCodeGenField FieldDefinition {fieldType = field@TypeRef {..}, ..} =
  CodeGenField
    { fieldName,
      fieldType = typeConName,
      wrappers = [GQL_WRAPPER typeWrappers],
      fieldIsNullable = isNullable field
    }

mkConsEnum :: TypeName -> DataEnumValue s -> CodeGenConstructor
mkConsEnum typename DataEnumValue {enumName} =
  CodeGenConstructor (CodeGenTypeName [coerce typename] [] enumName) []
