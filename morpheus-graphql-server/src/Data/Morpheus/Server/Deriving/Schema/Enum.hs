{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Server.Deriving.Schema.Enum
  ( buildEnumTypeContent,
    defineEnumUnit,
  )
where

import Data.Morpheus.Server.Deriving.Schema.Directive
  ( UseDirective,
    deriveEnumDirectives,
    visitEnumName,
    visitEnumValueDescription,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
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
  )

buildEnumTypeContent :: gql a => UseDirective gql args -> CatType kind a -> [TypeName] -> SchemaT k (TypeContent TRUE kind CONST)
buildEnumTypeContent options p@InputType enumCons = DataEnum <$> traverse (mkEnumValue options p) enumCons
buildEnumTypeContent options p@OutputType enumCons = DataEnum <$> traverse (mkEnumValue options p) enumCons

mkEnumValue :: gql a => UseDirective gql args -> f a -> TypeName -> SchemaT k (DataEnumValue CONST)
mkEnumValue options proxy enumName = do
  enumDirectives <- deriveEnumDirectives options proxy enumName
  pure
    DataEnumValue
      { enumName = visitEnumName options proxy enumName,
        enumDescription = visitEnumValueDescription options proxy enumName Nothing,
        ..
      }

defineEnumUnit :: SchemaT cat ()
defineEnumUnit =
  insertType
    ( mkType unitTypeName (mkEnumContent [unitTypeName]) ::
        TypeDefinition LEAF CONST
    )
