{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.Field where

import           Data.Data                       (Data)
import           Data.Morpheus.Schema.InputValue (InputValue)
import           Data.Text                       (Text)
import           GHC.Generics

data Field t = Field
  { name              :: Text
  , description       :: Text
  , args              :: [InputValue t]
  , _type             :: Maybe t
  , isDeprecated      :: Bool
  , deprecationReason :: Text
  } deriving (Show, Data, Generic)

createFieldWith :: Text -> a -> [InputValue a] -> Field a
createFieldWith _name fieldType fieldArgs =
  Field
    { name = _name
    , description = ""
    , args = fieldArgs
    , _type = Just fieldType
    , isDeprecated = False
    , deprecationReason = ""
    }
