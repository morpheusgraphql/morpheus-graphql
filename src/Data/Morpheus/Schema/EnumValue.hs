{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Morpheus.Schema.EnumValue
  ( EnumValue(..)
  , createEnumValue
  , isEnumOf
  ) where

import           Data.Morpheus.Kind.Internal (KIND, OBJECT)
import           Data.Text                   (Text)
import           GHC.Generics

type instance KIND EnumValue = OBJECT

data EnumValue = EnumValue
  { name              :: Text
  , description       :: Maybe Text
  , isDeprecated      :: Bool
  , deprecationReason :: Maybe Text
  } deriving (Generic)

createEnumValue :: Text -> EnumValue
createEnumValue enumName =
  EnumValue {name = enumName, description = Nothing, isDeprecated = False, deprecationReason = Nothing}

isEnumValue :: Text -> EnumValue -> Bool
isEnumValue inpName enum = inpName == name enum

isEnumOf :: Text -> [EnumValue] -> Bool
isEnumOf enumName enumValues =
  case filter (isEnumValue enumName) enumValues of
    [] -> False
    _  -> True
