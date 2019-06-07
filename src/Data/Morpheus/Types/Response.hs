{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Response
  ( GQLResponse(..)
  ) where

import           Data.Aeson                              (ToJSON (..), pairs, (.=))
import           Data.Morpheus.Types.Internal.Validation (JSONError (..))
import           Data.Morpheus.Types.Internal.Value      (Value)
import           GHC.Generics                            (Generic)

data GQLResponse
  = Data Value
  | Errors [JSONError]
  deriving (Show, Generic)

instance ToJSON GQLResponse where
  toEncoding (Data _data)     = pairs $ "data" .= _data
  toEncoding (Errors _errors) = pairs $ "errors" .= _errors
