{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.GQL__Field where

import           Data.Data                            (Data)
import           Data.Morpheus.Schema.GQL__InputValue (GQL__InputValue)
import           Data.Text                            (Text (..))
import           GHC.Generics

data GQL__Field t = GQL__Field
  { name              :: Text
  , description       :: Text
  , args              :: [GQL__InputValue t]
  , _type             :: Maybe t
  , isDeprecated      :: Bool
  , deprecationReason :: Text
  } deriving (Show, Data, Generic)

createFieldWith :: Text -> a -> [GQL__InputValue a] -> GQL__Field a
createFieldWith _name fieldType args =
  GQL__Field
    {name = _name, description = "", args = args, _type = Just fieldType, isDeprecated = False, deprecationReason = ""}
