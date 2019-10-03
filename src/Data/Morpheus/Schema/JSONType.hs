{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Data.Morpheus.Schema.JSONType
  ( JSONType(..)
  , JSONSchema(..)
  , JSONIntro(..)
  ) where

import           Data.Aeson
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

--
-- MORPHEUS
import qualified Data.Morpheus.Schema.Field      as F (Field (..))
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind)

newtype JSONIntro = JSONIntro
  { __schema :: JSONSchema
  } deriving (Generic, Show, FromJSON)

data JSONType = JSONType
  { kind          :: TypeKind
  , name          :: Maybe Text
  , description   :: Maybe Text
  , fields        :: Maybe [F.Field JSONType]
  , interfaces    :: Maybe [JSONType]
  , possibleTypes :: Maybe [JSONType]
--  , enumValues    :: Maybe [EnumValue]
  , inputFields   :: Maybe [I.InputValue JSONType]
  , ofType        :: Maybe JSONType
  } deriving (Generic, Show, FromJSON)

newtype JSONSchema = JSONSchema
  { types      :: [JSONType]
  } deriving (Generic, Show, FromJSON)
