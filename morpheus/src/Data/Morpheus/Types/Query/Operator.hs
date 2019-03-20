module Data.Morpheus.Types.Query.Operator
  ( Operator(..)
  , ValidOperator
  , RawOperator
  ) where

import           Data.Morpheus.Types.Core               (Key)
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.RawSelection (RawArguments, RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Arguments, SelectionSet)

type ValidOperator = Operator Arguments SelectionSet

type RawOperator = Operator RawArguments RawSelectionSet

data Operator args sel
  = Query Key
          args
          sel
          Position
  | Mutation Key
             args
             sel
             Position
