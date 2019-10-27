{-# LANGUAGE DeriveLift #-}

module Data.Morpheus.Types.Internal.AST.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , SelectionRec(..)
  , ArgumentOrigin(..)
  , ValidSelection
  , Selection(..)
  ) where

import           Data.Morpheus.Types.Internal.Base  (Collection, Key, Position)
import           Data.Morpheus.Types.Internal.Value (Value)
import           Language.Haskell.TH.Syntax         (Lift (..))

data ArgumentOrigin
  = VARIABLE
  | INLINE
  deriving (Show, Lift)

data Argument = Argument
  { argumentValue    :: Value
  , argumentOrigin   :: ArgumentOrigin
  , argumentPosition :: Position
  } deriving (Show, Lift)

type Arguments = Collection Argument

type SelectionSet = Collection ValidSelection

type ValidSelection = Selection Arguments SelectionRec

type UnionSelection = Collection SelectionSet

data Selection args rec = Selection
  { selectionArguments :: args
  , selectionPosition  :: Position
  , selectionAlias     :: Maybe Key
  , selectionRec       :: rec
  } deriving (Show)

data SelectionRec
  = SelectionSet SelectionSet
  | UnionSelection UnionSelection
  | SelectionField
  deriving (Show)
