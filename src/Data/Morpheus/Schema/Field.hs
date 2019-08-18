{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Schema.Field where

import           Data.Morpheus.Kind                 (OBJECT)
import           Data.Morpheus.Schema.InputValue    (InputValue)
import           Data.Morpheus.Types.GQLType        (GQLType (KIND, __typeName, __typeVisibility))
import           Data.Morpheus.Types.Internal.Value (convertToJSONName)
import           Data.Text                          (Text)
import           Data.Typeable                      (Typeable)
import           GHC.Generics

instance Typeable a => GQLType (Field a) where
  type KIND (Field a) = OBJECT
  __typeName = const "__Field"
  __typeVisibility = const False

data Field t =
  Field
    { name              :: Text
    , description       :: Maybe Text
    , args              :: [InputValue t]
    , type'             :: t
    , isDeprecated      :: Bool
    , deprecationReason :: Maybe Text
    }
  deriving (Generic)

createFieldWith :: Text -> a -> [InputValue a] -> Field a
createFieldWith _name fieldType fieldArgs =
  Field
    { name = convertToJSONName _name
    , description = Nothing
    , args = fieldArgs
    , type' = fieldType
    , isDeprecated = False
    , deprecationReason = Nothing
    }
