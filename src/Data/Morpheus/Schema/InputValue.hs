{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.InputValue
  ( InputValue(..)
  , createInputValueWith
  ) where

import           Data.Morpheus.Kind          (KIND, OBJECT)
import           Data.Morpheus.Types.GQLType (GQLType (__typeName))
import           Data.Text                   (Text)
import           Data.Typeable               (Typeable)
import           GHC.Generics

type instance KIND (InputValue a) = OBJECT

instance Typeable a => GQLType (InputValue a) where
  __typeName = const "__InputValue"

data InputValue t = InputValue
  { name         :: Text
  , description  :: Maybe Text
  , type'        :: t
  , defaultValue :: Maybe Text
  } deriving (Generic)

createInputValueWith :: Text -> a -> InputValue a
createInputValueWith _name ofType =
  InputValue {name = _name, description = Nothing, type' = ofType, defaultValue = Nothing}
