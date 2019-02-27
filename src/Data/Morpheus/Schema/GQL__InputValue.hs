{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable , DeriveGeneric #-}

module Data.Morpheus.Schema.GQL__InputValue (GQL__InputValue(..), createInputValueWith ) where

import           Data.Text                      ( Text(..))
import           Data.Data                      ( Data )
import           GHC.Generics

data GQL__InputValue t = GQL__InputValue {
  name:: Text,
  description::  Text,
  _type:: Maybe t,
  defaultValue::  Text
} deriving (Show , Data, Generic)

createInputValueWith :: Text -> a -> GQL__InputValue a
createInputValueWith _name ofType = GQL__InputValue
  { name         = _name
  , description  = ""
  , _type        = Just ofType
  , defaultValue = ""
  }