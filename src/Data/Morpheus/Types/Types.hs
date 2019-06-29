module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Variables
  ) where

import           Data.Map                                      (Map)
import           Data.Morpheus.Types.Internal.AST.Operator     (RawOperator)
import           Data.Morpheus.Types.Internal.AST.RawSelection (FragmentLib)
import           Data.Morpheus.Types.Internal.Base             (Key)
import           Data.Morpheus.Types.Internal.Value            (Value)

type Variables = Map Key Value

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , queryBody      :: RawOperator
  , inputVariables :: [(Key, Value)]
  }
