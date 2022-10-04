{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Server.Deriving.Schema.Enum
  ( buildEnumTypeContent,
    defineEnumUnit,
  )
where

import Data.Morpheus.Server.Deriving.Schema.Directive
  ( deriveEnumDirectives,
    visitEnumValueName,
  )
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( lookupDescription,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( KindedType (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    insertType,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataEnumValue (..),
    LEAF,
    TRUE,
    TypeContent (..),
    TypeDefinition,
    TypeName,
    mkEnumContent,
    mkType,
    unitTypeName,
    unpackName,
  )

buildEnumTypeContent :: GQLType a => KindedType kind a -> [TypeName] -> SchemaT c (TypeContent TRUE kind CONST)
buildEnumTypeContent p@InputType enumCons = DataEnum <$> traverse (mkEnumValue p) enumCons
buildEnumTypeContent p@OutputType enumCons = DataEnum <$> traverse (mkEnumValue p) enumCons

mkEnumValue :: GQLType a => f a -> TypeName -> SchemaT c (DataEnumValue CONST)
mkEnumValue proxy enumName = do
  enumDirectives <- deriveEnumDirectives proxy enumName
  let name = visitEnumValueName proxy enumName
  pure
    DataEnumValue
      { enumName = name,
        enumDescription = lookupDescription proxy (unpackName enumName),
        ..
      }

defineEnumUnit :: SchemaT cat ()
defineEnumUnit =
  insertType
    ( mkType unitTypeName (mkEnumContent [unitTypeName]) ::
        TypeDefinition LEAF CONST
    )
