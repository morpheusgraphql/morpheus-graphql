{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Internal.AST
  ( DerivingClass (..),
    TypeValue (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
    unpackName,
  )
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
  deriving (Show)

instance Pretty DerivingClass where
  pretty SHOW = "Show"
  pretty GENERIC = "Generic"

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
