{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Data.Morpheus.Types.JSType where

import           Data.Map                       ( Map
                                                , filter
                                                , mapKeys
                                                , fromList
                                                )
import           Data.Aeson
import qualified Data.Text                   as T ( Text, pack )
import           GHC.Generics                   ( Generic )
import          Data.HashMap.Strict             (toList)


replaceType :: T.Text -> T.Text
replaceType "_type" = "type"
replaceType x = x

data JSType =  JSObject [(T.Text, JSType)] | JSList [JSType] |  JSEnum T.Text | JSInt Int | JSBool Bool | JSString T.Text | JSNull  deriving (Show, Generic)


instance ToJSON JSType where
    toJSON JSNull = Null
    toJSON (JSInt x) = toJSON x
    toJSON (JSBool x) = toJSON x
    toJSON (JSString x) = toJSON x
    toJSON (JSList x) = toJSON x
    toJSON (JSObject x) = object (map convertField x)
       where convertField (key, value ) = replaceType key .= toJSON value

    -- toEncoding (JSObject x) = pairs $ foldr (<>) (head $ tail fields) (tail fields)
    --   where
    --    fields = map encodeField x
    --    encodeField (key, value ) = key .= value
replace (key, val ) = (key, replaceValue val )

replaceValue :: Value -> JSType
replaceValue (Bool v) = JSBool v
replaceValue (Number v) = JSInt 0 -- TODO: fix number from 0 to actual value
replaceValue (String v) = JSString v
replaceValue (Object v) = JSObject  $ map replace (toList v)

instance FromJSON JSType where
    parseJSON = pure . replaceValue
