{-# LANGUAGE  OverloadedStrings , DeriveGeneric , DeriveAnyClass , DeriveDataTypeable  #-}

module Data.Morpheus.Schema.GQL__EnumValue
  (GQL__EnumValue(..),createEnumValue)
where

import           Data.Text                      (Text)
import           GHC.Generics
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Data                      ( Data )

data  GQL__EnumValue = GQL__EnumValue{
  name:: Text
  ,description::Text
  ,isDeprecated:: Bool
  ,deprecationReason:: Text
} deriving (Show , Data, Generic )

createEnumValue :: Text -> GQL__EnumValue
createEnumValue name = GQL__EnumValue {
    name = name
    ,description = ""
    ,isDeprecated = False
    ,deprecationReason = ""
}