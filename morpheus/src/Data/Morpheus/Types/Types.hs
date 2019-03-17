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

import           Data.Map                            (Map)
import           Data.Morpheus.Types.Core            (Key)
import           Data.Morpheus.Types.JSType          (JSType)
import           Data.Morpheus.Types.Query.Fragment  (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.Selection (Argument (..), Arguments, QuerySelection (..),
                                                      SelectionSet)

data GQLOperator
  = QueryOperator Key
                  QuerySelection
  | MutationOperator Key
                     QuerySelection

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: GQLOperator
  , inputVariables :: Map Key JSType
  }
