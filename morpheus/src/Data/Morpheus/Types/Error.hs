{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Error
  ( GQLError(..)
  , ErrorLocation(..)
  , GQLErrors
  , JSONError(..)
  ) where

import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data GQLError = GQLError
  { desc     :: Text
  , posIndex :: Int
  } deriving (Show)

type GQLErrors = [GQLError]

data ErrorLocation = ErrorLocation
  { line   :: Int
  , column :: Int
  } deriving (Show, Generic, ToJSON)

data JSONError = JSONError
  { message   :: Text
  , locations :: [ErrorLocation]
  } deriving (Show, Generic, ToJSON)
