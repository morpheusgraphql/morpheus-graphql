{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Internal.Validation
  ( GQLError(..)
  , Location(..)
  , GQLErrors
  , JSONError(..)
  , Validation
  , ResolveValue
  )
where

import           Control.Monad.Trans.Except     ( ExceptT(..) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Morpheus.Types.Internal.Base
                                                ( Location(..) )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data GQLError = GQLError
  { desc      :: Text
  , positions :: [Location]
  } deriving (Show)

type GQLErrors = [GQLError]

data JSONError = JSONError
  { message   :: Text
  , locations :: [Location]
  } deriving (Show, Generic, FromJSON, ToJSON)

type Validation = Either GQLErrors

type ResolveValue m = ExceptT GQLErrors m Value
