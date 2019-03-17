{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.InputValue
  ( InputValue(..)
  , createInputValueWith
  , isRequired
  , inputValueMeta
  ) where

import           Data.Data                    (Data)
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..), Position)
import           Data.Text                    (Text)
import           GHC.Generics

data InputValue t = InputValue
  { name         :: Text
  , description  :: Text
  , _type        :: Maybe t
  , defaultValue :: Text
  } deriving (Show, Data, Generic)

isRequired :: InputValue a -> Bool
isRequired x = defaultValue x /= "Nothing"

inputValueMeta :: Position -> InputValue a -> MetaInfo
inputValueMeta pos input = MetaInfo {typeName = "TODO: Type", key = name input, position = pos}

createInputValueWith :: Text -> a -> InputValue a
createInputValueWith _name ofType = InputValue {name = _name, description = "", _type = Just ofType, defaultValue = ""}
