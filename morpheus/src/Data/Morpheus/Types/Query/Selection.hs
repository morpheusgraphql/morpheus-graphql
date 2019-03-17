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

data QuerySelection
  = SelectionSet Arguments
                 SelectionSet
                 Position
  | Field Arguments
          Key
          Position
  | Spread Key
           Position
  | QNull
  deriving (Show)
