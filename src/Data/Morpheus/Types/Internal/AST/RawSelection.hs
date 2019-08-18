{-# LANGUAGE DeriveLift #-}

module Data.Morpheus.Types.Internal.AST.RawSelection
  ( Reference(..)
  , Argument(..)
  , RawArgument(..)
  , RawSelection(..)
  , Fragment(..)
  , RawSelection'(..)
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  ) where

import           Data.Morpheus.Types.Internal.AST.Selection (Argument (..))
import           Data.Morpheus.Types.Internal.Base          (Collection, Key, Position)
import           Data.Text                                  (Text)
import           Language.Haskell.TH.Syntax                 (Lift (..))

data Reference = Reference
  { referenceName     :: Key
  , referencePosition :: Position
  } deriving (Show, Lift)

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show, Lift)

data RawSelection' a = RawSelection'
  { rawSelectionArguments :: RawArguments
  , rawSelectionPosition  :: Position
  , rawSelectionRec       :: a
  } deriving (Show, Lift)

type FragmentLib = [(Key, Fragment)]

data RawArgument
  = VariableReference Reference
  | RawArgument Argument
  deriving (Show, Lift)

type RawArguments = Collection RawArgument

type RawSelectionSet = Collection RawSelection

data RawSelection
  = RawSelectionSet (RawSelection' RawSelectionSet)
  | RawSelectionField (RawSelection' ())
  | InlineFragment Fragment
  | Spread Reference
  | RawAlias { rawAliasPosition  :: Position
             , rawAliasSelection :: (Key, RawSelection) }
  deriving (Show, Lift)
