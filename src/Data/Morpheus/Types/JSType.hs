{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Data.Morpheus.Types.JSType where

import           Data.Map                       ( Map
                                                , filter
                                                , mapKeys
                                                , fromList
                                                )
import           Data.Aeson                      ( ToJSON(..)
                                                , FromJSON(..)
                                                , Value(..))
import qualified Data.Text                   as T ( Text )
import           GHC.Generics                   ( Generic )
import          Data.HashMap.Strict             (toList)


replaceType :: T.Text -> T.Text
replaceType "_type" = "type"
replaceType x = x

data JSType =  JSObject (Map T.Text JSType)| JSList [JSType] |  JSEnum T.Text | JSInt Int | JSBool Bool | JSString T.Text | JSNull  deriving (Show, Generic)

instance ToJSON JSType where
    toJSON (JSInt x) = toJSON x
    toJSON (JSBool x) = toJSON x
    toJSON (JSString x) = toJSON x
    toJSON JSNull = Null
    toJSON (JSObject x) = toJSON (mapKeys replaceType x)
    toJSON (JSList x) = toJSON x

replace (key, val ) = (key, replaceValue val )

replaceValue :: Value -> JSType
replaceValue (Bool v) = JSBool v
replaceValue (Number v) = JSInt 0 -- TODO: fix number from 0 to actual value
replaceValue (String v) = JSString v
replaceValue (Object v) = JSObject $ fromList $ map replace (toList v)

instance FromJSON JSType where
    parseJSON = pure . replaceValue
