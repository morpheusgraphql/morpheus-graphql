{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Types.Types
  ( QuerySelection(..)
  , SelectionSet
  , GQLQueryRoot(..)
  , Fragment(..)
  , FragmentLib
  , GQLResponse(..)
  , GQLRequest(..)
  , Argument(..)
  , Arguments
  , GQLOperator(..)
  ) where

import           Data.Aeson                   (FromJSON (..), ToJSON (..), pairs, (.=))
import           Data.Map                     (Map)
import           Data.Morpheus.Types.Core     (Key)
import           Data.Morpheus.Types.Error    (JSONError (..))
import           Data.Morpheus.Types.JSType   (JSType)
import           Data.Morpheus.Types.MetaInfo (Position)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)

data Argument
  = Variable Key
             Position
  | Argument JSType
             Position
  deriving (Show)

type Arguments = [(Key, Argument)]

type SelectionSet = [(Key, QuerySelection)]

data QuerySelection
  = SelectionSet Arguments
                 SelectionSet
                 Position
  | Field Arguments
          Key
          Position
  | Spread Key
           Position
  | QNull
  deriving (Show)

data GQLOperator
  = QueryOperator Text
                  QuerySelection
  | MutationOperator Text
                     QuerySelection

type FragmentLib = Map Text Fragment

data Fragment = Fragment
  { id              :: Text
  , target          :: Text
  , fragmentContent :: QuerySelection
  } deriving (Show)

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: GQLOperator
  , inputVariables :: Map Text JSType
  }

data GQLResponse
  = Data JSType
  | Errors [JSONError]
  deriving (Show, Generic)

instance ToJSON GQLResponse where
  toEncoding (Data _data)     = pairs $ "data" .= _data
  toEncoding (Errors _errors) = pairs $ "errors" .= _errors

data GQLRequest = GQLRequest
  { query         :: Text
  , operationName :: Maybe Text
  , variables     :: Maybe (Map Text JSType)
  } deriving (Show, Generic, FromJSON)
