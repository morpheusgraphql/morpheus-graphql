{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Parsing.JSONSchema.Types
  ( Introspection (..),
    Schema (..),
    Type (..),
    Field (..),
    InputValue (..),
    EnumValue (..),
  )
where

import Data.Aeson
--
-- MORPHEUS
import Data.Morpheus.Schema.TypeKind (TypeKind)
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
  )
import GHC.Generics (Generic)

-- TYPES FOR DECODING JSON INTROSPECTION
--
newtype Introspection = Introspection
  { __schema :: Schema
  }
  deriving (Generic, Show, FromJSON)

newtype Schema = Schema
  { types :: [Type]
  }
  deriving (Generic, Show, FromJSON)

-- TYPE
data Type = Type
  { kind :: TypeKind,
    name :: Maybe TypeName,
    fields :: Maybe [Field],
    interfaces :: Maybe [Type],
    possibleTypes :: Maybe [Type],
    enumValues :: Maybe [EnumValue],
    inputFields :: Maybe [InputValue],
    ofType :: Maybe Type
  }
  deriving (Generic, Show, FromJSON)

-- FIELD
data Field = Field
  { fieldName :: FieldName,
    fieldArgs :: [InputValue],
    fieldType :: Type
  }
  deriving (Show, Generic)

instance FromJSON Field where
  parseJSON = withObject "Field" objectParser
    where
      objectParser o = Field <$> o .: "name" <*> o .: "args" <*> o .: "type"

-- INPUT
data InputValue = InputValue
  { inputName :: FieldName,
    inputType :: Type
  }
  deriving (Show, Generic)

instance FromJSON InputValue where
  parseJSON = withObject "InputValue" objectParser
    where
      objectParser o = InputValue <$> o .: "name" <*> o .: "type"

-- ENUM
newtype EnumValue = EnumValue
  { enumName :: TypeName
  }
  deriving (Generic, Show)

instance FromJSON EnumValue where
  parseJSON = withObject "EnumValue" objectParser
    where
      objectParser o = EnumValue <$> o .: "name"
