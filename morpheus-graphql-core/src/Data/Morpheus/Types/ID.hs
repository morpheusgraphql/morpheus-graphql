{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.ID
  ( ID (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Morpheus.Types.GQLScalar
  ( GQLScalar (..),
    scalarFromJSON,
    scalarToJSON,
  )
import Data.Morpheus.Types.Internal.AST
  ( ScalarValue (..),
  )
import Data.Text
  ( Text,
    pack,
  )
import GHC.Generics (Generic)

-- | default GraphQL type,
-- parses only 'String' and 'Int' values,
-- serialized always as 'String'
newtype ID = ID
  { unpackID :: Text
  }
  deriving (Show, Generic)

instance GQLScalar ID where
  parseValue (Int x) = return (ID $ pack $ show x)
  parseValue (String x) = return (ID x)
  parseValue _ = Left ""
  serialize (ID x) = String x

instance ToJSON ID where
  toJSON = scalarToJSON

instance FromJSON ID where
  parseJSON = scalarFromJSON
