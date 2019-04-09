module Data.Morpheus.Types.Query.Operator
  ( Operator(..)
  , ValidOperator
  , RawOperator
  , Variable(..)
  , VariableDefinitions
  ) where

import           Data.Morpheus.Types.Core               (Collection, Key)
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Arguments, SelectionSet)

type ValidOperator = Operator Arguments SelectionSet

data Variable =
  Variable Key
           Position

type VariableDefinitions = Collection Variable

type RawOperator = Operator VariableDefinitions RawSelectionSet

data Operator args sel
  = Query Key
          args
          sel
          Position
  | Mutation Key
             args
             sel
             Position
