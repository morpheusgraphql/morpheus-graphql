{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Internal
  ( GQLTypeOptions (..),
    defaultTypeOptions,
    TypeData (..),
    prefixInputs,
    mkTypeData,
  )
where

-- MORPHEUS

import Data.Morpheus.Server.Types.SchemaT
  ( TypeFingerprint (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeName,
    TypeWrapper (..),
    mkBaseType,
  )
import Relude hiding (Seq, Undefined, intercalate)

data TypeData = TypeData
  { gqlTypeName :: TypeName,
    gqlWrappers :: TypeWrapper,
    gqlFingerprint :: TypeFingerprint
  }
  deriving (Show)

-- | Options that specify how to map GraphQL field, type, and constructor names
-- to and from their Haskell equivalent.
--
-- Options can be set using record syntax on 'defaultOptions' with the fields
-- below.
data GQLTypeOptions = GQLTypeOptions
  { -- | Function applied to field labels.
    -- Handy for removing common record prefixes for example.
    fieldLabelModifier :: String -> String,
    -- | Function applied to constructor tags.
    constructorTagModifier :: String -> String,
    -- | Construct a new type name depending on whether it is an input,
    -- and being given the original type name.
    typeNameModifier :: Bool -> String -> String
  }

-- | Default encoding 'GQLTypeOptions':
--
-- @
-- 'GQLTypeOptions'
--   { 'fieldLabelModifier'      = id
--   , 'constructorTagModifier'  = id
--   , 'typeNameModifier'        = const id
--   }
-- @
defaultTypeOptions :: GQLTypeOptions
defaultTypeOptions =
  GQLTypeOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id,
      -- default is just a pass through for the original type name
      typeNameModifier = const id
    }

prefixInputs :: GQLTypeOptions -> GQLTypeOptions
prefixInputs options = options {typeNameModifier = \isInput name -> if isInput then "Input" <> name else name}

mkTypeData :: TypeName -> a -> TypeData
mkTypeData name _ =
  TypeData
    { gqlTypeName = name,
      gqlFingerprint = InternalFingerprint name,
      gqlWrappers = mkBaseType
    }
