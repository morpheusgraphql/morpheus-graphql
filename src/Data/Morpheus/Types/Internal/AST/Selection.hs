module Data.Morpheus.Types.Internal.AST.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , Selection(..)
  ) where

import           Data.Morpheus.Types.Internal.Base  (Collection, Position)
import           Data.Morpheus.Types.Internal.Value (Value)

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
