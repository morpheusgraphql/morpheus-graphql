{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Types.Internal.AST.Operation
  ( Operation(..)
  , Variable(..)
  , ValidOperation
  , RawOperation
  , VariableDefinitions
  , ValidVariables
  , DefaultValue
  ) where

import           Data.Morpheus.Types.Internal.AST.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Internal.AST.Selection    (Arguments, SelectionSet)
import           Data.Morpheus.Types.Internal.Base             (Collection, Key, Position)
import           Data.Morpheus.Types.Internal.Data             (OperationKind, WrapperD)
import           Data.Morpheus.Types.Internal.TH               (apply, liftText, liftTextMap)
import           Data.Morpheus.Types.Internal.Value            (Value)
import           Language.Haskell.TH.Syntax                    (Lift (..))

type DefaultValue = Maybe Value

type VariableDefinitions = Collection (Variable DefaultValue)

type ValidVariables = Collection (Variable Value)

type ValidOperation = Operation Arguments SelectionSet

type RawOperation = Operation VariableDefinitions RawSelectionSet

data Operation args sel = Operation
  { operationName      :: Key
  , operationKind      :: OperationKind
  , operationArgs      :: args
  , operationSelection :: sel
  , operationPosition  :: Position
  } deriving (Show)

instance Lift (Operation VariableDefinitions RawSelectionSet) where
  lift (Operation name kind args sel pos) =
    apply 'Operation [liftText name, lift kind, liftTextMap args, liftTextMap sel, lift pos]

data Variable a = Variable
  { variableType         :: Key
  , isVariableRequired   :: Bool
  , variableTypeWrappers :: [WrapperD]
  , variablePosition     :: Position
  , variableValue        :: a
  } deriving (Show)

instance Lift a => Lift (Variable a) where
  lift (Variable t ir w p v) = apply 'Variable [liftText t, lift ir, lift w, lift p, lift v]
