{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.GQL__EnumValue
  ( GQL__EnumValue(..)
  , createEnumValue
  , isEnumOf
  ) where


import           Data.Data                  (Data)
import           Data.Text                  (Text)
import           GHC.Generics

data GQL__EnumValue = GQL__EnumValue
  { name              :: Text
  , description       :: Text
  , isDeprecated      :: Bool
  , deprecationReason :: Text
  } deriving (Show, Data, Generic)

createEnumValue :: Text -> GQL__EnumValue
createEnumValue name = GQL__EnumValue {name = name, description = "", isDeprecated = False, deprecationReason = ""}

isEnumValue :: Text -> GQL__EnumValue -> Bool
isEnumValue inpName enum = inpName == name enum

isEnumOf :: Text -> [GQL__EnumValue] -> Bool
isEnumOf name enumValues =
  case filter (isEnumValue name) enumValues of
    [] -> False
    _  -> True
