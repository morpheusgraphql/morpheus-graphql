{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Error
  ( GQLError(..)
  , ErrorLocation(..)
  , GQLErrors
  , JSONError(..)
  , InputError(..)
  , InputValidation
  ) where

import           Data.Aeson                   (ToJSON)
import           Data.Morpheus.Types.MetaInfo (MetaInfo)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)

data InputError
  = TypeMismatch MetaInfo
                 Text
                 Text
  | UndefinedType MetaInfo
  | UndefinedField MetaInfo
  | UnknownField MetaInfo
  | UnknownType MetaInfo

type InputValidation a = Either InputError a

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
