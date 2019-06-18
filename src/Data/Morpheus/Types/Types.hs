module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Variables
  , GQLRootResolver(..)
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

-- | GraphQL root resolver, Interpreter generates GQL schema from it
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver a b c = GQLRootResolver
  { queryResolver        :: a
  , mutationResolver     :: b
  , subscriptionResolver :: c
  }
