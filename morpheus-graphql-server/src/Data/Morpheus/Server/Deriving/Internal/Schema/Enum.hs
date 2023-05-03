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
    getEnumDirectives,
    serializeDirectives,
    visitEnumName,
    visitEnumValueDescription,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataEnumValue (..),
    TRUE,
    TypeContent (..),
    TypeName,
  )

buildEnumTypeContent :: (gql a) => UseDeriving gql args -> CatType kind a -> [TypeName] -> GQLResult (TypeContent TRUE kind CONST)
buildEnumTypeContent options p@InputType enumCons = DataEnum <$> traverse (mkEnumValue options p) enumCons
buildEnumTypeContent options p@OutputType enumCons = DataEnum <$> traverse (mkEnumValue options p) enumCons

mkEnumValue :: (gql a) => UseDeriving gql args -> f a -> TypeName -> GQLResult (DataEnumValue CONST)
mkEnumValue ctx proxy enumName = do
  enumDirectives <- serializeDirectives ctx (getEnumDirectives ctx proxy enumName)
  pure
    DataEnumValue
      { enumName = visitEnumName ctx proxy enumName,
        enumDescription = visitEnumValueDescription ctx proxy enumName Nothing,
        ..
      }
