{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Error
  ( GQLError(..)
  , ErrorLocation(..)
  , GQLErrors
  , JSONError(..)
  , Validation
  , ResolveIO
  , failResolveIO
  ) where

import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Aeson                 (ToJSON)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

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

type Validation a = Either GQLErrors a

type ResolveIO = ExceptT GQLErrors IO

failResolveIO :: GQLErrors -> ResolveIO a
failResolveIO = ExceptT . pure . Left
