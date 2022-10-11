{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Internal.AST
  ( DerivingClass (..),
    TypeValue (..),
    CodeGenField (..),
    FIELD_TYPE_WRAPPER (..),
    prefixTypeName,
    CodeGenTypeName (..),
    CodeGenConstructor (..),
    CodeGenType (..),
    getFullName,
    fromTypeName,
    CGName (..),
  )
where

import Data.Morpheus.CodeGen.Internal.Name (camelCaseTypeName)
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
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

data CodeGenType = CodeGenType
  { cgTypeName :: CodeGenTypeName,
    cgConstructors :: [CodeGenConstructor],
    cgDerivations :: [DerivingClass]
  }
  deriving (Show)

data CodeGenConstructor = CodeGenConstructor
  { constructorName :: CodeGenTypeName,
    constructorFields :: [CodeGenField]
  }
  deriving (Show)

data CGName = CGName
  { cgnNamespaces :: [Text],
    cgnName :: Text
  }
  deriving (Show)

data CodeGenField = CodeGenField
  { fieldName :: FieldName,
    fieldType :: TypeName,
    wrappers :: [FIELD_TYPE_WRAPPER],
    fieldIsNullable :: Bool
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

data CodeGenTypeName = CodeGenTypeName
  { namespace :: [FieldName],
    typeParameters :: [Text],
    typename :: TypeName
  }
  deriving (Show)

prefixTypeName :: CodeGenTypeName -> TypeName -> CodeGenTypeName
prefixTypeName CodeGenTypeName {..} = CodeGenTypeName namespace [] . camelCaseTypeName [typename]

getFullName :: CodeGenTypeName -> TypeName
getFullName CodeGenTypeName {..} = camelCaseTypeName namespace typename

fromTypeName :: TypeName -> CodeGenTypeName
fromTypeName = CodeGenTypeName [] []
