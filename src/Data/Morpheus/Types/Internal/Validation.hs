{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Internal.Validation
  ( GQLError(..)
  , Position(..)
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
                                                ( Position(..) )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data GQLError = GQLError
  { desc      :: Text
  , positions :: [Position]
  } deriving (Show)

type GQLErrors = [GQLError]

data JSONError = JSONError
  { message   :: Text
  , locations :: [Position]
  } deriving (Show, Generic, FromJSON, ToJSON)

type Validation = Either GQLErrors

type ResolveValue m = ExceptT GQLErrors m Value
