{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable , DeriveGeneric #-}

module Data.Morpheus.Schema.GQL__Field where


import           Data.Text                      ( Text(..) )
import           Data.Data                      ( Data )
import           GHC.Generics
import           Data.Morpheus.Schema.GQL__InputValue
                                                ( GQL__InputValue )

data GQL__Field t = GQL__Field{
  name:: Text,
  description:: Text,
  args:: [GQL__InputValue t],
  _type :: Maybe t,
  isDeprecated:: Bool,
  deprecationReason :: Text
} deriving (Show , Data, Generic)

createFieldWith :: Text -> a -> [GQL__InputValue a] -> GQL__Field a
createFieldWith _name fieldType args = GQL__Field
  { name              = _name
  , description       = ""
  , args              = args
  , _type             = Just fieldType
  , isDeprecated      = False
  , deprecationReason = ""
  }
