{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Server.Deriving.Internal.Schema.Enum
  ( buildEnumTypeContent,
  )
where

import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( UseDeriving,
    deriveEnumDirectives,
    visitEnumName,
    visitEnumValueDescription,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( SchemaBuilder (runSchemaT),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataEnumValue (..),
    TRUE,
    TypeContent (..),
    TypeName,
  )

buildEnumTypeContent :: gql a => UseDeriving gql args -> CatType kind a -> [TypeName] -> GQLResult (TypeContent TRUE kind CONST)
buildEnumTypeContent options p@InputType enumCons = DataEnum <$> traverse (mkEnumValue options p) enumCons
buildEnumTypeContent options p@OutputType enumCons = DataEnum <$> traverse (mkEnumValue options p) enumCons

mkEnumValue :: gql a => UseDeriving gql args -> f a -> TypeName -> GQLResult (DataEnumValue CONST)
mkEnumValue options proxy enumName = do
  enumDirectives <- fst <$> runSchemaT (deriveEnumDirectives options proxy enumName)
  pure
    DataEnumValue
      { enumName = visitEnumName options proxy enumName,
        enumDescription = visitEnumValueDescription options proxy enumName Nothing,
        ..
      }
