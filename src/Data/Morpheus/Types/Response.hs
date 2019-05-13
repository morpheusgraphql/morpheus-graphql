{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Response where

import           Data.Aeson                 (ToJSON (..), pairs, (.=))
import           Data.Morpheus.Types.Error  (JSONError (..))
import           Data.Morpheus.Types.JSType (JSType)
import           GHC.Generics               (Generic)

data GQLResponse
  = Data JSType
  | Errors [JSONError]
  deriving (Show, Generic)

instance ToJSON GQLResponse where
  toEncoding (Data _data)     = pairs $ "data" .= _data
  toEncoding (Errors _errors) = pairs $ "errors" .= _errors
