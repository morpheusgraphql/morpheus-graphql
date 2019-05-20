module Data.Morpheus.Types.Query.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , Selection(..)
  ) where

import           Data.Morpheus.Types.Core           (Collection)
import           Data.Morpheus.Types.Internal.Value (Value)
import           Data.Morpheus.Types.MetaInfo       (Position)

data Argument =
  Argument Value
           Position
  deriving (Show)

type Arguments = Collection Argument

type SelectionSet = Collection Selection

data Selection
  = SelectionSet Arguments
                 SelectionSet
                 Position
  | UnionSelection Arguments
                   (Collection SelectionSet)
                   Position
  | SelectionField Arguments
                   Position
  deriving (Show)
