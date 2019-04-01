{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Error
  ( GQLError(..)
  , ErrorLocation(..)
  , GQLErrors
  , JSONError(..)
  , MetaError(..)
  , MetaValidation
  , Validation
  , ResolveIO
  , failResolveIO
  ) where

import           Control.Monad.Trans.Except   (ExceptT (..))
import           Data.Aeson                   (ToJSON)
import           Data.Morpheus.Types.JSType   (JSType)
import           Data.Morpheus.Types.MetaInfo (MetaInfo)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)

data MetaError
  = TypeMismatch MetaInfo
                 JSType
                 Text
  | UnknownField MetaInfo
  | UnknownType MetaInfo

data GQLError = GQLError
  { desc     :: Text
  , posIndex :: [Int]
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

type MetaValidation a = Either MetaError a

type Validation a = Either GQLErrors a

type ResolveIO = ExceptT GQLErrors IO

failResolveIO :: GQLErrors -> ResolveIO a
failResolveIO = ExceptT . pure . Left
