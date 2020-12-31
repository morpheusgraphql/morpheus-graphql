{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.ID
  ( ID (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Hashable
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
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
  deriving
    ( Show,
      Generic,
      Eq,
      Hashable,
      IsString
    )

instance DecodeScalar ID where
  decodeScalar (Int x) = pure (ID $ pack $ show x)
  decodeScalar (String x) = pure (ID x)
  decodeScalar _ = Left "ID can only be String or number"

instance EncodeScalar ID where
  encodeScalar (ID x) = String x

instance ToJSON ID where
  toJSON = scalarToJSON

instance FromJSON ID where
  parseJSON = scalarFromJSON
