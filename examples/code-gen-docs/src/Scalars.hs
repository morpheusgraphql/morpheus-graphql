{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Scalars where

import Data.Morpheus.Client.CodeGen.Internal
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
  ( DecodeScalar (..),
    EncodeScalar (..),
    GQLType (KIND),
    ScalarValue (..),
  )
import Data.Text (Text)

newtype Markdown = Markdown Text
  deriving
    ( Show,
      Generic,
      Eq
    )

instance GQLType Markdown where
  type KIND Markdown = SCALAR

instance DecodeScalar Markdown where
  decodeScalar (String x) = pure (Markdown x)
  decodeScalar _ = fail "not suppoorted"

instance EncodeScalar Markdown where
  encodeScalar (Markdown x) = String x

instance FromJSON Markdown where
  parseJSON = scalarFromJSON

instance ToJSON Markdown where
  toJSON = scalarToJSON