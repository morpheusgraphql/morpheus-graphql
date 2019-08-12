module Data.Morpheus.Types.Internal.AST.Operator
  ( Operator(..)
  , ValidOperator
  , RawOperator
  , Variable(..)
  , VariableDefinitions
  , Operator'(..)
  , ValidOperator'
  , RawOperator'
  , ValidVariables
  , unpackOperator
  ) where

import           Data.Morpheus.Types.Internal.AST.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Internal.AST.Selection    (Arguments, SelectionSet)
import           Data.Morpheus.Types.Internal.Base             (Collection, Key, Position)
import           Data.Morpheus.Types.Internal.Data             (DataTypeWrapper)
import           Data.Morpheus.Types.Internal.Value            (Value)

type ValidOperator = Operator Arguments SelectionSet

type ValidOperator' = Operator' Arguments SelectionSet

data Variable a = Variable
  { variableType         :: Key
  , isVariableRequired   :: Bool
  , variableTypeWrappers :: [DataTypeWrapper]
  , variablePosition     :: Position
  , variableValue        :: a
  } deriving (Show)

type VariableDefinitions = Collection (Variable ())

type ValidVariables = Collection (Variable Value)

type RawOperator = Operator VariableDefinitions RawSelectionSet

type RawOperator' = Operator' VariableDefinitions RawSelectionSet

data Operator' args sel = Operator'
  { operatorName      :: Key
  , operatorArgs      :: args
  , operatorSelection :: sel
  , operatorPosition  :: Position
  } deriving (Show)

unpackOperator :: Operator a b -> Operator' a b
unpackOperator (Query x)        = x
unpackOperator (Mutation x)     = x
unpackOperator (Subscription x) = x

data Operator args sel
  = Query (Operator' args sel)
  | Mutation (Operator' args sel)
  | Subscription (Operator' args sel)
  deriving (Show)
