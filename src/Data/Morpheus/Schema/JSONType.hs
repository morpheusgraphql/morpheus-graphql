{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.JSONType
  ( JSONType(..)
  , JSONSchema(..)
  , JSONIntro(..)
  , JSONResponse(..)
  ) where

import           Data.Aeson
import           Data.Morpheus.Schema.Directive  (Directive)
import           Data.Morpheus.Schema.EnumValue  (EnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..))
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

instance FromJSON JSONResponse where
  parseJSON = withObject "JSONResponse" objectParser
    where
      objectParser o = JSONResponse <$> o .: "data"

newtype JSONResponse = JSONResponse
  { responseData :: JSONIntro
  } deriving (Generic, Show)

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
  , enumValues    :: Maybe [EnumValue]
  , inputFields   :: Maybe [I.InputValue JSONType]
  , ofType        :: Maybe JSONType
  } deriving (Generic, Show, FromJSON)

data JSONSchema = JSONSchema
  { types      :: [JSONType]
   --, queryType        :: JSONType
 -- , mutationType     :: Maybe JSONType
 -- , subscriptionType :: Maybe JSONType
  , directives :: [Directive JSONType]
  } deriving (Generic, Show, FromJSON)
