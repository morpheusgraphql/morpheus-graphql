{-# LANGUAGE DeriveGeneric #-}

module Data.Morpheus.Schema.InputValue
  ( InputValue(..)
  , createInputValueWith
  ) where

import           Data.Text    (Text)
import           GHC.Generics

data InputValue t = InputValue
  { name         :: Text
  , description  :: Maybe Text
  , _type        :: t
  , defaultValue :: Maybe Text
  } deriving (Generic)

createInputValueWith :: Text -> a -> InputValue a
createInputValueWith _name ofType =
  InputValue {name = _name, description = Nothing, _type = ofType, defaultValue = Nothing}
