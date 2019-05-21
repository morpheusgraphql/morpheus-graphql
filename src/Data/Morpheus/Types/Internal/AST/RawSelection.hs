module Data.Morpheus.Types.Internal.AST.RawSelection
  ( Reference(..)
  , Argument(..)
  , RawArgument(..)
  , RawSelection(..)
  , Fragment(..)
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  ) where

import           Data.Map                                   (Map)
import           Data.Morpheus.Types.Internal.AST.Selection (Argument (..))
import           Data.Morpheus.Types.Internal.Base          (Collection, Key, Position)

data Reference = Reference
  { referenceName     :: Key
  , referencePosition :: Position
  } deriving (Show)

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show)

type FragmentLib = Map Key Fragment

data RawArgument
  = VariableReference Reference
  | RawArgument Argument
  deriving (Show)

type RawArguments = Collection RawArgument

type RawSelectionSet = Collection RawSelection

data RawSelection
  = RawSelectionSet RawArguments
                    RawSelectionSet
                    Position
  | RawField RawArguments
             Key
             Position
  | InlineFragment Fragment
  | Spread Reference
  deriving (Show)
