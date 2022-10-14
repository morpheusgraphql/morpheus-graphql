{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Internal
  ( GQLTypeOptions (..),
    defaultTypeOptions,
    TypeData (..),
    prefixInputs,
    mkTypeData,
    dropNamespaceOptions,
    stripConstructorNamespace,
    stripFieldNamespace,
  )
where

-- MORPHEUS

import Data.Char (toLower)
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint (..))
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
    TypeName,
    TypeWrapper (..),
    mkBaseType,
  )
import Data.Text (length)
import Relude hiding (Seq, Undefined, intercalate, length)

data TypeData = TypeData
  { gqlTypeName :: TypeName,
    gqlWrappers :: TypeWrapper,
    gqlFingerprint :: TypeFingerprint
  }
  deriving (Show)

{-# DEPRECATED GQLTypeOptions "use: custom directives with 'VisitType'" #-}

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

{-# DEPRECATED defaultTypeOptions "use: custom directives with 'VisitType'" #-}

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

{-# DEPRECATED prefixInputs "use: custom directives" #-}
prefixInputs :: GQLTypeOptions -> GQLTypeOptions
prefixInputs options = options {typeNameModifier = \isInput name -> if isInput then "Input" <> name else name}

mkTypeData :: TypeName -> a -> TypeData
mkTypeData name _ =
  TypeData
    { gqlTypeName = name,
      gqlFingerprint = InternalFingerprint name,
      gqlWrappers = mkBaseType
    }

dropPrefix :: Text -> String -> String
dropPrefix name = drop (length name)

stripConstructorNamespace :: Text -> String -> String
stripConstructorNamespace = dropPrefix

stripFieldNamespace :: Text -> String -> String
stripFieldNamespace prefix = __uncapitalize . dropPrefix prefix
  where
    __uncapitalize [] = []
    __uncapitalize (x : xs) = toLower x : xs

{-# DEPRECATED dropNamespaceOptions "use: custom directives" #-}
dropNamespaceOptions :: TypeKind -> Text -> GQLTypeOptions -> GQLTypeOptions
dropNamespaceOptions KindInterface tName opt =
  opt
    { typeNameModifier = const (stripConstructorNamespace "Interface"),
      fieldLabelModifier = stripFieldNamespace tName
    }
dropNamespaceOptions KindEnum tName opt = opt {constructorTagModifier = stripConstructorNamespace tName}
dropNamespaceOptions _ tName opt = opt {fieldLabelModifier = stripFieldNamespace tName}
