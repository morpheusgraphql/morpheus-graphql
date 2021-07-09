{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Server.Deriving.Schema.Enum
  ( buildEnumTypeContent,
    defineEnumUnit,
  )
where

import Data.Morpheus.Server.Deriving.Schema.Internal
  ( lookupDescription,
    lookupDirectives,
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
import Data.Morpheus.Utils.Kinded
  ( KindedType (..),
  )

buildEnumTypeContent :: GQLType a => KindedType kind a -> [TypeName] -> SchemaT c (TypeContent TRUE kind CONST)
buildEnumTypeContent p@InputType enumCons = pure $ DataEnum $ map (mkEnumValue p) enumCons
buildEnumTypeContent p@OutputType enumCons = pure $ DataEnum $ map (mkEnumValue p) enumCons

mkEnumValue :: GQLType a => f a -> TypeName -> DataEnumValue CONST
mkEnumValue proxy enumName =
  DataEnumValue
    { enumName,
      enumDescription = lookupDescription proxy (unpackName enumName),
      enumDirectives = lookupDirectives proxy (unpackName enumName)
    }

defineEnumUnit :: SchemaT cat ()
defineEnumUnit =
  insertType
    ( mkType unitTypeName (mkEnumContent [unitTypeName]) ::
        TypeDefinition LEAF CONST
    )
