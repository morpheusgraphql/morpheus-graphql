{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.EnumValue
  ( EnumValue(..)
  , createEnumValue
  , isEnumOf
  ) where

import           Data.Data    (Data)
import           Data.Text    (Text)
import           GHC.Generics

data EnumValue = EnumValue
  { name              :: Text
  , description       :: Text
  , isDeprecated      :: Bool
  , deprecationReason :: Text
  } deriving (Show, Data, Generic)

createEnumValue :: Text -> EnumValue
createEnumValue enumName = EnumValue {name = enumName, description = "", isDeprecated = False, deprecationReason = ""}

isEnumValue :: Text -> EnumValue -> Bool
isEnumValue inpName enum = inpName == name enum

isEnumOf :: Text -> [EnumValue] -> Bool
isEnumOf enumName enumValues =
  case filter (isEnumValue enumName) enumValues of
    [] -> False
    _  -> True
