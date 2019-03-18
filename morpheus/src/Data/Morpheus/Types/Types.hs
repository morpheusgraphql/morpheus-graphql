module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Fragment(..)
  , FragmentLib
  ) where

import           Data.Map                           (Map)
import           Data.Morpheus.Types.Core           (Key)
import           Data.Morpheus.Types.JSType         (JSType)
import           Data.Morpheus.Types.Query.Fragment (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.Operator (RawOperator)

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: RawOperator
  , inputVariables :: Map Key JSType
  }
