{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Internal.Validation
  ( GQLError(..)
  , ErrorLocation(..)
  , GQLErrors
  , JSONError(..)
  , Validation
  , ResolveT
  , failResolveT
  , SchemaValidation
  , ResolveValue
  ) where

import           Control.Monad.Trans.Except         (ExceptT (..))
import           Data.Aeson                         (ToJSON)
import           Data.Morpheus.Types.Internal.Base  (Position)
import           Data.Morpheus.Types.Internal.Value (Value)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

data GQLError =
  GQLError
    { desc      :: Text
    , positions :: [Position]
    }
  deriving (Show)

type GQLErrors = [GQLError]

data ErrorLocation =
  ErrorLocation
    { line   :: Int
    , column :: Int
    }
  deriving (Show, Generic, ToJSON)

data JSONError =
  JSONError
    { message   :: Text
    , locations :: [ErrorLocation]
    }
  deriving (Show, Generic, ToJSON)

type Validation a = Either GQLErrors a

type SchemaValidation a = Validation a

type ResolveT = ExceptT GQLErrors

type ResolveValue m = ExceptT GQLErrors m Value

failResolveT :: Monad m => GQLErrors -> ResolveT m a
failResolveT = ExceptT . pure . Left
