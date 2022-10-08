{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Internal.AST
  ( DerivingClass (..),
    TypeValue (..),
    CodeGenField (..),
    FIELD_TYPE_WRAPPER (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    Name,
    TypeName,
    TypeRef,
    TypeWrapper,
    unpackName,
  )
import qualified Language.Haskell.TH.Syntax as TH
import Prettyprinter
  ( Doc,
    Pretty (..),
    punctuate,
    vsep,
    (<+>),
  )
import Relude

data DerivingClass
  = SHOW
  | GENERIC
  | CLASS_EQ
  deriving (Show)

instance Pretty DerivingClass where
  pretty SHOW = "Show"
  pretty GENERIC = "Generic"
  pretty CLASS_EQ = "Eq"

data TypeValue
  = TypeValueObject TypeName [(FieldName, TypeValue)]
  | TypeValueNumber Double
  | TypeValueString Text
  | TypeValueBool Bool
  | TypeValueList [TypeValue]
  | TypedValueMaybe (Maybe TypeValue)
  deriving (Show)

renderField :: (FieldName, TypeValue) -> Doc n
renderField (fName, fValue) = pretty (unpackName fName :: Text) <> "=" <+> pretty fValue

instance Pretty TypeValue where
  pretty (TypeValueObject name xs) =
    pretty (unpackName name :: Text)
      <+> "{"
      <+> vsep (punctuate "," (map renderField xs))
      <+> "}"
  pretty (TypeValueNumber x) = pretty x
  pretty (TypeValueString x) = pretty (show x :: String)
  pretty (TypeValueBool x) = pretty x
  pretty (TypedValueMaybe (Just x)) = "Just" <+> pretty x
  pretty (TypedValueMaybe Nothing) = "Nothing"
  pretty (TypeValueList xs) = prettyList xs

data CodeGenField = CodeGenField
  { fieldType :: Text,
    fieldName :: FieldName,
    wrappers :: [FIELD_TYPE_WRAPPER]
  }
  deriving (Show)

data FIELD_TYPE_WRAPPER
  = MONAD
  | SUBSCRIPTION TH.Name
  | PARAMETRIZED
  | ARG TypeName
  | TAGGED_ARG TH.Name FieldName TypeRef
  | GQL_WRAPPER TypeWrapper
  deriving (Show)