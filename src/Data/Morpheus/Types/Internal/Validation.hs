{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Internal.Validation
  ( GQLError(..)
  , Location(..)
  , GQLErrors
  , JSONError(..)
  , Validation
  , ResolveT
  , failResolveT
  , SchemaValidation
  , ResolveValue
  ) where

import           Control.Monad.Trans.Except         (ExceptT (..))
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Morpheus.Types.Internal.Base  (Location (..))
import           Data.Morpheus.Types.Internal.Value (Value)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

data GQLError = GQLError
  { desc      :: Text
  , positions :: [Location]
  } deriving (Show)

type GQLErrors = [GQLError]

data JSONError = JSONError
  { message   :: Text
  , locations :: [Location]
  } deriving (Show, Generic, FromJSON, ToJSON)

type Validation a = Either GQLErrors a

type SchemaValidation a = Validation a

type ResolveT = ExceptT GQLErrors

type ResolveValue m = ExceptT GQLErrors m Value

failResolveT :: Monad m => GQLErrors -> ResolveT m a
failResolveT = ExceptT . pure . Left
