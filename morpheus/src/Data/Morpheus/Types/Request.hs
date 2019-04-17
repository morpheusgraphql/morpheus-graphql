{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Request where

import           Data.Aeson                 (FromJSON (..))
import           Data.Map                   (Map)
import           Data.Morpheus.Types.Core   (Key)
import           Data.Morpheus.Types.JSType (JSType)
import           GHC.Generics               (Generic)

data GQLRequest = GQLRequest
  { query         :: Key
  , operationName :: Maybe Key
  , variables     :: Maybe (Map Key JSType)
  } deriving (Show, Generic, FromJSON)
