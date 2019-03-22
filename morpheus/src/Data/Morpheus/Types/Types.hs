module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  ) where

import           Data.Map                           (Map)
import           Data.Morpheus.Types.Core           (Key)
import           Data.Morpheus.Types.JSType         (JSType)
import           Data.Morpheus.Types.Query.Fragment (FragmentLib)
import           Data.Morpheus.Types.Query.Operator (RawOperator)

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: RawOperator
  , inputVariables :: Map Key JSType
  }
