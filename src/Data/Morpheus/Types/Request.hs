{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Request (GQLRequest(..)) where

import           Data.Aeson                         (FromJSON (..))
import           Data.Map                           (Map)
import           Data.Morpheus.Types.Internal.Base  (Key)
import           Data.Morpheus.Types.Internal.Value (Value)
import           GHC.Generics                       (Generic)

-- | GraphQL HTTP Request Body
data GQLRequest = GQLRequest
  { query         :: Key
  , operationName :: Maybe Key
  , variables     :: Maybe (Map Key Value)
  } deriving (Show, Generic, FromJSON)
