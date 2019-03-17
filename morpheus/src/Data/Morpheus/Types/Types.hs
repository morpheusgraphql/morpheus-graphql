module Data.Morpheus.Types.Types
  ( QuerySelection(..)
  , SelectionSet
  , GQLQueryRoot(..)
  , Fragment(..)
  , FragmentLib
  , Argument(..)
  , Arguments
  , GQLOperator(..)
  ) where

import           Data.Map                     (Map)
import           Data.Morpheus.Types.Core     (Key)
import           Data.Morpheus.Types.JSType   (JSType)
import           Data.Morpheus.Types.MetaInfo (Position)

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
  = QueryOperator Key
                  QuerySelection
  | MutationOperator Key
                     QuerySelection

type FragmentLib = Map Key Fragment

data Fragment = Fragment
  { id              :: Key
  , target          :: Key
  , fragmentContent :: QuerySelection
  } deriving (Show)

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: GQLOperator
  , inputVariables :: Map Key JSType
  }
