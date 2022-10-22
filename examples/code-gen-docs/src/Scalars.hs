{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Scalars where

import Data.Morpheus.Client.CodeGen.Internal
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
  ( DecodeScalar (..),
    EncodeScalar (..),
    GQLType (KIND),
    ScalarValue (Int),
  )

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