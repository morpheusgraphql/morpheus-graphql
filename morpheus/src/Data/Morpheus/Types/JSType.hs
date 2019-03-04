{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Morpheus.Types.JSType where

import           Data.Aeson                     ( ToJSON(..)
                                                , (.=)
                                                , FromJSON(..)
                                                , Value(..)
                                                , pairs
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )
import           Data.HashMap.Strict            ( toList )

replaceType :: Text -> Text
replaceType "_type" = "type"
replaceType x       = x

data JSType =  JSObject [(Text, JSType)] | JSList [JSType] |  JSEnum Text | JSInt Int | JSBool Bool | JSString Text | JSNull  deriving (Show, Generic)

instance ToJSON JSType where
    toEncoding JSNull = toEncoding Null
    toEncoding (JSInt x) = toEncoding x
    toEncoding (JSBool x) = toEncoding x
    toEncoding (JSString x) = toEncoding x
    toEncoding (JSList x) = toEncoding x
    toEncoding (JSObject x) = pairs $ foldl1 (<>) $ map encodeField x
       where encodeField (key, value ) = replaceType key .= value

replace (key, val) = (key, replaceValue val)

replaceValue :: Value -> JSType
replaceValue (Bool   v) = JSBool v
replaceValue (Number v) = JSInt 0 -- TODO: fix number from 0 to actual value
replaceValue (String v) = JSString v
replaceValue (Object v) = JSObject $ map replace (toList v)

instance FromJSON JSType where
    parseJSON = pure . replaceValue
