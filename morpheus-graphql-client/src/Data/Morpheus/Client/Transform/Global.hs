{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Global
  ( toGlobalDefinitions,
  )
where

import Data.Morpheus.Client.Internal.AST
  ( ClientPreDeclaration,
    ClientTypeDefinition (..),
  )
import Data.Morpheus.Client.Transform.Core (toClientDeclarations, toCodeGenField)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenTypeName (..),
    fromTypeName,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DataEnumValue (DataEnumValue, enumName),
    Schema (Schema, types),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    VALID,
    isNotSystemTypeName,
    isResolverType,
  )
import Relude hiding (empty)

toGlobalDefinitions :: (TypeName -> Bool) -> Schema VALID -> [ClientPreDeclaration]
toGlobalDefinitions f Schema {types} =
  concatMap toClientDeclarations $
    mapMaybe generateGlobalType $
      filter shouldInclude (sortWith typeName $ toList types)
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
