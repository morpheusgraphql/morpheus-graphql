{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.ID
  ( ID (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Morpheus.Types.GQLScalar
  ( ScalarDecoder (..),
    ScalarEncoder (..),
    scalarFromJSON,
    scalarToJSON,
  )
import Data.Morpheus.Types.Internal.AST
  ( ScalarValue (..),
  )
import Data.Text (pack)
import Relude

-- | default GraphQL type,
-- parses only 'String' and 'Int' values,
-- serialized always as 'String'
newtype ID = ID
  { unpackID :: Text
  }
  deriving (Show, Generic, Eq)

instance ScalarDecoder ID where
  parseValue (Int x) = pure (ID $ pack $ show x)
  parseValue (String x) = pure (ID x)
  parseValue _ = Left "ID can only be String or number"

instance ScalarEncoder ID where
  serialize (ID x) = String x

instance ToJSON ID where
  toJSON = scalarToJSON

instance FromJSON ID where
  parseJSON = scalarFromJSON
