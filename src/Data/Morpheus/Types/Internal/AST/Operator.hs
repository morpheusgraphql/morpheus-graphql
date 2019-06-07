module Data.Morpheus.Types.Internal.AST.Operator
  ( Operator(..)
  , ValidOperator
  , RawOperator
  , Variable(..)
  , VariableDefinitions
  , Operator'(..)
  , ValidOperator'
  , RawOperator'
  ) where

import           Data.Morpheus.Types.Internal.AST.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Internal.AST.Selection    (Arguments, SelectionSet)
import           Data.Morpheus.Types.Internal.Base             (Collection, Key, Position)
import           Data.Morpheus.Types.Internal.Data             (DataTypeWrapper)

type ValidOperator = Operator Arguments SelectionSet

type ValidOperator' = Operator' Arguments SelectionSet

data Variable = Variable
  { variableType         :: Key
  , isVariableRequired   :: Bool
  , variableTypeWrappers :: [DataTypeWrapper]
  , variablePosition     :: Position
  } deriving (Show)

type VariableDefinitions = Collection Variable

type RawOperator = Operator VariableDefinitions RawSelectionSet

type RawOperator' = Operator' VariableDefinitions RawSelectionSet

data Operator' args sel = Operator'
  { operatorName      :: Key
  , operatorArgs      :: args
  , operatorSelection :: sel
  , operatorPosition  :: Position
  } deriving (Show)

data Operator args sel
  = Query (Operator' args sel)
  | Mutation (Operator' args sel)
  | Subscription (Operator' args sel)
  deriving (Show)
