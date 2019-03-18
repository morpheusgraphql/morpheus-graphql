module Data.Morpheus.Types.Query.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , QuerySelection(..)
  ) where

import           Data.Morpheus.Types.Core     (Key)
import           Data.Morpheus.Types.JSType   (JSType)
import           Data.Morpheus.Types.MetaInfo (Position)

data Argument
  = Variable Key
             Position
  | Argument JSType
             Position
  deriving (Show)

type Arguments = [(Key, Argument)]

type SelectionSet = [(Key, QuerySelection)]

-- TODO: define 2 Types raw and validated selections
data QuerySelection
  = SelectionSet Arguments
                 SelectionSet
                 Position
  | Field Arguments
          Key
          Position
  | Spread Key
           Position
  deriving (Show) -- data ValArgument = ValArgument JSType Position
-- type OutSet = [(Key, OutSelection)]
-- data OutSelection = OutSet ValArgument OutSet Position | OutField Arguments Key Position | Null
