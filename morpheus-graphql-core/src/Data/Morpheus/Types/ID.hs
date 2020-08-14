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
import Prelude
  ( ($),
    Either (..),
    Eq,
    Show (..),
    pure,
  )

-- | default GraphQL type,
-- parses only 'String' and 'Int' values,
-- serialized always as 'String'
newtype ID = ID
  { unpackID :: Text
  }
  deriving (Show, Generic, Eq)

instance GQLScalar ID where
  parseValue (Int x) = pure (ID $ pack $ show x)
  parseValue (String x) = pure (ID x)
  parseValue _ = Left "ID can only be String or number"
  serialize (ID x) = String x

instance ToJSON ID where
  toJSON = scalarToJSON

instance FromJSON ID where
  parseJSON = scalarFromJSON
