module Data.Morpheus.Types.Internal.AST.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , Selection(..)
  , SelectionRec(..)
  , ArgumentOrigin(..)
  ) where

import           Data.Morpheus.Types.Internal.Base  (Collection, Key, Position)
import           Data.Morpheus.Types.Internal.Value (Value)

data ArgumentOrigin
  = VARIABLE
  | INLINE
  deriving (Show)

data Argument = Argument
  { argumentValue    :: Value
  , argumentOrigin   :: ArgumentOrigin
  , argumentPosition :: Position
  } deriving (Show)

type Arguments = Collection Argument

type SelectionSet = Collection Selection

type UnionSelection = Collection SelectionSet

data Selection = Selection
  { selectionArguments :: Arguments
  , selectionPosition  :: Position
  , selectionRec       :: SelectionRec
  } deriving (Show)

data SelectionRec
  = SelectionSet SelectionSet
  | UnionSelection UnionSelection
  | SelectionAlias { aliasFieldName :: Key
                   , aliasSelection :: SelectionRec }
  | SelectionField
  deriving (Show)
