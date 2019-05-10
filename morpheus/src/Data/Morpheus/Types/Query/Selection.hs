module Data.Morpheus.Types.Query.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , Selection(..)
  ) where

import           Data.Morpheus.Types.Core     (Collection, Key)
import           Data.Morpheus.Types.JSType   (JSType)
import           Data.Morpheus.Types.MetaInfo (Position)

data Argument =
  Argument JSType
           Position
  deriving (Show)

type Arguments = Collection Argument

type SelectionSet = Collection Selection

data Selection
  = SelectionSet Arguments
                 SelectionSet
                 Position
  | Field Arguments
          Key
          Position
  | Spread Key
           Position
  deriving (Show)
