{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Data.Morpheus.Schema.Field where

import           Data.Data                       (Data)
import           Data.Morpheus.Schema.InputValue (InputValue)
import           Data.Text                       (Text)
import           GHC.Generics

data Field t = Field
  { name              :: Text
  , description       :: Maybe Text
  , args              :: [InputValue t]
  , _type             :: t
  , isDeprecated      :: Bool
  , deprecationReason :: Maybe Text
  } deriving (Show, Data, Generic)

createFieldWith :: Text -> a -> [InputValue a] -> Field a
createFieldWith _name fieldType fieldArgs =
  Field
    { name = _name
    , description = Nothing
    , args = fieldArgs
    , _type = fieldType
    , isDeprecated = False
    , deprecationReason = Nothing
    }
