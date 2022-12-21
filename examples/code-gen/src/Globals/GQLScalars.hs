{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Globals.GQLScalars where

import Data.Morpheus.Client.CodeGen.Internal
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
  ( DecodeScalar (..),
    EncodeScalar (..),
    GQLType (..),
    ScalarValue (..),
  )
import Data.Text (Text)

data TestScalar
  = TestScalar
      Int
      Int
  deriving (Show, Generic)

instance GQLType TestScalar where
  type KIND TestScalar = SCALAR

instance DecodeScalar TestScalar where
  decodeScalar _ = pure (TestScalar 1 0)

instance EncodeScalar TestScalar where
  encodeScalar (TestScalar x y) = Int (x * 100 + y)

newtype ScalarPower = ScalarPower Text deriving (Show, Generic)

instance GQLType ScalarPower where
  type KIND ScalarPower = SCALAR

instance DecodeScalar ScalarPower where
  decodeScalar (String x) = pure (ScalarPower x)
  decodeScalar _ = fail "only strings!"

instance EncodeScalar ScalarPower where
  encodeScalar (ScalarPower x) = String x

instance FromJSON ScalarPower where
  parseJSON = scalarFromJSON

instance ToJSON ScalarPower where
  toJSON = scalarToJSON

newtype Euro = Euro Int
  deriving
    ( Show,
      Eq
    )

instance GQLType Euro where
  type KIND Euro = SCALAR

instance DecodeScalar Euro where
  decodeScalar _ = pure $ Euro 1

instance EncodeScalar Euro where
  encodeScalar (Euro x) = Int x

instance FromJSON Euro where
  parseJSON = scalarFromJSON

instance ToJSON Euro where
  toJSON = scalarToJSON
