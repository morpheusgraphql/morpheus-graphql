{-# LANGUAGE DeriveLift #-}

module Data.Morpheus.Types.Internal.AST.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , SelectionRec(..)
  , ValueOrigin(..)
  , ValidSelection
  , Selection(..)
  , RawSelection'
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  , Fragment(..)
  , RawArgument(..)
  , RawSelection(..)
  , Reference(..)
  )
where

import           Language.Haskell.TH.Syntax     ( Lift )


-- MORPHEUS
import           Data.Morpheus.Types.Internal.Base
                                                ( Collection
                                                , Key
                                                , Position
                                                , Reference(..)
                                                )

import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )


-- RAW SELECTION

type RawSelection' a = Selection RawArguments a

type FragmentLib = [(Key, Fragment)]

type RawArguments = Collection RawArgument

type RawSelectionSet = Collection RawSelection

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show, Lift)

data RawArgument
  = VariableReference Reference
  | RawArgument Argument
  deriving (Show, Lift)


data RawSelection
  = RawSelectionSet (RawSelection' RawSelectionSet)
  | RawSelectionField (RawSelection' ())
  | InlineFragment Fragment
  | Spread Reference
  deriving (Show,Lift)

-- VALID SELECTION 
type Arguments = Collection Argument

type SelectionSet = Collection ValidSelection

type ValidSelection = Selection Arguments SelectionRec

type UnionSelection = Collection SelectionSet

data ValueOrigin
  = VARIABLE
  | INLINE
  deriving (Show, Lift)

data Argument = Argument
  { argumentValue    :: Value
  , argumentOrigin   :: ValueOrigin
  , argumentPosition :: Position
  } deriving (Show, Lift)

data Selection args rec = Selection
  { selectionArguments :: args
  , selectionPosition  :: Position
  , selectionAlias     :: Maybe Key
  , selectionRec       :: rec
  } deriving (Show,Lift)

data SelectionRec
  = SelectionSet SelectionSet
  | UnionSelection UnionSelection
  | SelectionField
  deriving (Show)
