{-# LANGUAGE DeriveLift #-}

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
import           Language.Haskell.TH.Syntax         (Lift)
import           Text.Megaparsec                    (SourcePos)

instance Lift SourcePos

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
