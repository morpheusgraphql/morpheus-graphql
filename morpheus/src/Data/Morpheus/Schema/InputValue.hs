{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.InputValue
  ( InputValue(..)
  , createInputValueWith
  ) where

import           Data.Data    (Data)
import           Data.Text    (Text)
import           GHC.Generics

data InputValue t = InputValue
  { name         :: Text
  , description  :: Text
  , _type        :: Maybe t
  , defaultValue :: Text
  } deriving (Show, Data, Generic)

createInputValueWith :: Text -> a -> InputValue a
createInputValueWith _name ofType = InputValue {name = _name, description = "", _type = Just ofType, defaultValue = ""}
