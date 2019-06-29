{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.IO
  ( GQLRequest(..)
  , GQLResponse(..)
  ) where

import           Data.Aeson                              (FromJSON (..), ToJSON (..), pairs, (.=))
import           Data.Map                                (Map)
import           GHC.Generics                            (Generic)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Base       (Key)
import           Data.Morpheus.Types.Internal.Validation (JSONError (..))
import           Data.Morpheus.Types.Internal.Value      (Value)

-- | GraphQL HTTP Request Body
data GQLRequest = GQLRequest
  { query         :: Key
  , operationName :: Maybe Key
  , variables     :: Maybe (Map Key Value)
  } deriving (Show, Generic, FromJSON)

-- | GraphQL Response
data GQLResponse
  = Data Value
  | Errors [JSONError]
  deriving (Show, Generic)

instance ToJSON GQLResponse where
  toEncoding (Data _data)     = pairs $ "data" .= _data
  toEncoding (Errors _errors) = pairs $ "errors" .= _errors
