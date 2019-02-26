{-# LANGUAGE DeriveDataTypeable , DeriveGeneric #-}

module Data.Morpheus.Schema.GQL__Field where


import           Data.Text                      ( Text(..))
import           Data.Data                      ( Data )
import           GHC.Generics
import           Data.Morpheus.Schema.GQL__InputValue (GQL__InputValue)

data GQL__Field t = GQL__Field{
  name:: Text,
  description:: Text,
  args:: [GQL__InputValue t],
  _type :: Maybe t,
  isDeprecated:: Bool,
  deprecationReason :: Text
} deriving (Show , Data, Generic)
