{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Data.Morpheus.Types.JSType where

import           Data.Map                       ( Map
                                                , filter
                                                , mapKeys
                                                )
import           Data.Aeson                     ( ToJSON(..)
                                                , Value(Null)
  )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

replaceType :: Text -> Text
replaceType "_type" = "type"
replaceType x = x

data JSType =  JSObject (Map Text JSType)| JSList [JSType] |  JSEnum Text | JSInt Int | JSBool Bool | JSString Text | JSNull  deriving (Show, Generic)

instance ToJSON JSType where
    toJSON (JSInt x) = toJSON x
    toJSON (JSBool x) = toJSON x
    toJSON (JSString x) = toJSON x
    toJSON JSNull = Null
    toJSON (JSObject x) = toJSON (mapKeys replaceType x)
    toJSON (JSList x) = toJSON x

-- TODO: FromJSON instance for JSType?
