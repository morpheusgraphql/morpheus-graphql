module Data.Morpheus.Types.Internal.AST.Operation
  ( Operation(..)
  , Variable(..)
  , OperationKind(..)
  , ValidOperation
  , RawOperation
  , VariableDefinitions
  , ValidVariables
  ) where

import           Data.Morpheus.Types.Internal.AST.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Internal.AST.Selection    (Arguments, SelectionSet)
import           Data.Morpheus.Types.Internal.Base             (Collection, Key, Position)
import           Data.Morpheus.Types.Internal.Data             (DataTypeWrapper)
import           Data.Morpheus.Types.Internal.Value            (Value)

type VariableDefinitions = Collection (Variable ())

type ValidVariables = Collection (Variable Value)

type ValidOperation = Operation Arguments SelectionSet

type RawOperation = Operation VariableDefinitions RawSelectionSet

data OperationKind
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  deriving (Show)

data Operation args sel = Operation
  { operationName      :: Key
  , operationKind      :: OperationKind
  , operationArgs      :: args
  , operationSelection :: sel
  , operationPosition  :: Position
  } deriving (Show)

data Variable a = Variable
  { variableType         :: Key
  , isVariableRequired   :: Bool
  , variableTypeWrappers :: [DataTypeWrapper]
  , variablePosition     :: Position
  , variableValue        :: a
  } deriving (Show)
