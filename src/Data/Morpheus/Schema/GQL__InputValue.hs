{-# LANGUAGE DeriveDataTypeable , DeriveGeneric #-}
module Data.Morpheus.Schema.GQL__InputValue (GQL__InputValue(..)) where

import           Data.Text                      ( Text(..))
import           Data.Data                      ( Data )
import           GHC.Generics


data GQL__InputValue t = GQL__InputValue {
  name:: Text,
  description::  Text,
  _type:: Maybe t,
  defaultValue::  Text
} deriving (Show , Data, Generic)