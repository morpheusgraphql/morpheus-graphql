{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Morpheus.Schema.InputValue
  ( InputValue(..)
  , createInputValueWith
  ) where

import           Data.Morpheus.Kind.Internal (KIND, OBJECT)
import           Data.Text                   (Text)
import           GHC.Generics

type instance KIND (InputValue a) = OBJECT

data InputValue t = InputValue
  { name         :: Text
  , description  :: Maybe Text
  , _type        :: t
  , defaultValue :: Maybe Text
  } deriving (Generic)

createInputValueWith :: Text -> a -> InputValue a
createInputValueWith _name ofType =
  InputValue {name = _name, description = Nothing, _type = ofType, defaultValue = Nothing}
