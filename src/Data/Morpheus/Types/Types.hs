module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Variables
  , GQLRoot(..)
  , SubscriptionResolver(..)
  ) where

import           Data.Map                                      (Map)
import           Data.Morpheus.Types.Internal.AST.Operator     (RawOperator)
import           Data.Morpheus.Types.Internal.AST.RawSelection (FragmentLib)
import           Data.Morpheus.Types.Internal.Base             (Key)
import           Data.Morpheus.Types.Internal.Validation       (ResolveIO)
import           Data.Morpheus.Types.Internal.Value            (Value)

type Variables = Map Key Value

newtype SubscriptionResolver = SubscriptionResolver
  { unpackSubscriptionResolver :: () -> ResolveIO Value
  }

instance Show SubscriptionResolver where
  show = const "SubscriptionResolver"

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
