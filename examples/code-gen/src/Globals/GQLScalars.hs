{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Globals.GQLScalars where

import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
  ( DecodeScalar (..),
    EncodeScalar (..),
    GQLType (KIND),
    ScalarValue (Int),
  )
import GHC.Generics (Generic)

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
