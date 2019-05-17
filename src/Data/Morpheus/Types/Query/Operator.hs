module Data.Morpheus.Types.Query.Operator
  ( Operator(..)
  , ValidOperator
  , RawOperator
  , Variable(..)
  , VariableDefinitions
  , ListWrapper(..)
  ) where

import           Data.Morpheus.Types.Core               (Collection, Key)
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Arguments, SelectionSet)

type ValidOperator = Operator Arguments SelectionSet

newtype ListWrapper =
  ListWrapper Bool

data Variable =
  Variable [ListWrapper]
           Key
           Bool
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
  | Subscription Key
                 args
                 sel
                 Position
