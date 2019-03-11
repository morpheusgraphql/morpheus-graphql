{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.JSType where

import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      pairs, (.=))
import           Data.HashMap.Strict (toList)
import           Data.Scientific     (Scientific, floatingOrInteger)
import           Data.Text           (Text, pack)
import           GHC.Generics        (Generic)

replaceType :: Text -> Text
replaceType "_type" = "type"
replaceType x       = x

data JSType
  = JSObject [(Text, JSType)]
  | JSList [JSType]
  | JSEnum Text
  | JSInt Int
  | JSFloat Float
  | JSBool Bool
  | JSString Text
  | JSNull
  deriving (Show, Generic)

instance ToJSON JSType where
  toEncoding JSNull = toEncoding Null
  toEncoding (JSInt x) = toEncoding x
  toEncoding (JSBool x) = toEncoding x
  toEncoding (JSString x) = toEncoding x
  toEncoding (JSList x) = toEncoding x
  toEncoding (JSObject x) = pairs $ foldl1 (<>) $ map encodeField x
    where
      encodeField (key, value) = replaceType key .= value

replace (key, val) = (key, replaceValue val)

decodeSciefintic :: Scientific -> JSType
decodeSciefintic v =
  case floatingOrInteger v of
    Left float -> JSFloat float
    Right int  -> JSInt int

replaceValue :: Value -> JSType
replaceValue (Bool v)   = JSBool v
replaceValue (Number v) = decodeSciefintic v
replaceValue (String v) = JSString v
replaceValue (Object v) = JSObject $ map replace (toList v)

instance FromJSON JSType where
  parseJSON = pure . replaceValue
