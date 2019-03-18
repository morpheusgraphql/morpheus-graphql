module Data.Morpheus.Types.Types
  ( QuerySelection(..)
  , SelectionSet
  , GQLQueryRoot(..)
  , Fragment(..)
  , FragmentLib
  , Argument(..)
  , Arguments
  ) where

import           Data.Map                            (Map)
import           Data.Morpheus.Types.Core            (Key)
import           Data.Morpheus.Types.JSType          (JSType)
import           Data.Morpheus.Types.Query.Fragment  (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.Operator  (RawOperator)
import           Data.Morpheus.Types.Query.Selection (Argument (..), Arguments, QuerySelection (..),
                                                      SelectionSet)

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: RawOperator
  , inputVariables :: Map Key JSType
  }
