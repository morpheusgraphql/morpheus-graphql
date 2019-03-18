module Data.Morpheus.Types.Query.Operator
  ( Operator(..)
  ) where

import           Data.Morpheus.Types.Core            (Key)
import           Data.Morpheus.Types.MetaInfo        (Position)
import           Data.Morpheus.Types.Query.Selection (Arguments, SelectionSet)

data Operator
  = Query Key
          Arguments
          SelectionSet
          Position
  | Mutation Key
             Arguments
             SelectionSet
             Position
