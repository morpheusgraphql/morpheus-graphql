{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Data.Morpheus.Types.JSType where

import           Data.Map                       ( Map
                                                , filter
                                                , mapKeys
                                                )
import           Data.Aeson                      ( ToJSON(..)
                                                , FromJSON(..)
                                                , Value(..))
import qualified Data.Text                   as T ( Text )
import           GHC.Generics                   ( Generic )


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

instance FromJSON JSType where
    parseJSON (Bool v) = pure $ JSBool v
    parseJSON (Number v) = pure $ JSInt 0 -- TODO: fix number from 0 to actual value
    parseJSON (String v) = pure $ JSString v