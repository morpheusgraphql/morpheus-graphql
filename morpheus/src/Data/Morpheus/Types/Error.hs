{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Error
  ( GQLError(..)
  , ErrorLocation(..)
  , GQLErrors
  , JSONError(..)
  , MetaError(..)
  , MetaValidation
  ) where

import           Data.Aeson                   (ToJSON)
import           Data.Morpheus.Types.MetaInfo (MetaInfo)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)

data MetaError
  = TypeMismatch MetaInfo
                 Text
                 Text
  | UndefinedType MetaInfo
  | UndefinedField MetaInfo
  | UnknownField MetaInfo
  | UnknownType MetaInfo

type MetaValidation a = Either MetaError a

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
