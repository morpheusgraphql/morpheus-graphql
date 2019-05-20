module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Variables
  , GQLRoot(..)
  ) where

import           Data.Map                           (Map)
import           Data.Morpheus.Types.Core           (Key)
import           Data.Morpheus.Types.Internal.Value (Value)
import           Data.Morpheus.Types.Query.Fragment (FragmentLib)
import           Data.Morpheus.Types.Query.Operator (RawOperator)

type Variables = Map Key Value

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: RawOperator
  , inputVariables :: [(Key, Value)]
  }

data GQLRoot a b c = GQLRoot
  { query        :: a
  , mutation     :: b
  , subscription :: c
  }
