module Data.Morpheus.Types.Query.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , QuerySelection(..)
  ) where

import           Data.Morpheus.Types.Core     (Collection, Key)
import           Data.Morpheus.Types.JSType   (JSType)
import           Data.Morpheus.Types.MetaInfo (Position)

data Argument
  = Variable Key
             Position
  | Argument JSType
             Position
  deriving (Show)

type Arguments = Collection Argument

type SelectionSet = Collection QuerySelection

data QuerySelection
  = SelectionSet Arguments
                 SelectionSet
                 Position
  | Field Arguments
          Key
          Position
  deriving (Show)