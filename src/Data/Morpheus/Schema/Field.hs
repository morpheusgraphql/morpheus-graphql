{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Morpheus.Schema.Field where

import           Data.Morpheus.Kind                 (KIND, OBJECT)
import           Data.Morpheus.Schema.InputValue    (InputValue)
import           Data.Morpheus.Types.Internal.Value (replaceKeywordTypeForJson)
import           Data.Text                          (Text)
import           GHC.Generics

type instance KIND (Field a) = OBJECT

data Field t = Field
  { name              :: Text
  , description       :: Maybe Text
  , args              :: [InputValue t]
  , _type             :: t
  , isDeprecated      :: Bool
  , deprecationReason :: Maybe Text
  } deriving (Generic)

createFieldWith :: Text -> a -> [InputValue a] -> Field a
createFieldWith _name fieldType fieldArgs =
  Field
    { name = replaceKeywordTypeForJson _name
    , description = Nothing
    , args = fieldArgs
    , _type = fieldType
    , isDeprecated = False
    , deprecationReason = Nothing
    }
