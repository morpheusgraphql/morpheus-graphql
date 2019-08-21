{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Schema.JSONType
  ( JSONType(..)
  ) where

import           Data.Morpheus.Schema.EnumValue  (EnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..))
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

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
  } deriving (Generic)
