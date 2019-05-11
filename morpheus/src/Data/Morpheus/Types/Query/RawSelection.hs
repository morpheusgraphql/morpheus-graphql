module Data.Morpheus.Types.Query.RawSelection
  ( RawArgument(..)
  , RawArguments
  , RawSelectionSet
  , RawSelection(..)
  ) where

import           Data.Morpheus.Types.Core     (Collection, Key)
import           Data.Morpheus.Types.JSType   (JSType)
import           Data.Morpheus.Types.MetaInfo (Position)

data RawArgument
  = VariableReference Key
                      Position
  | Argument JSType
             Position
  deriving (Show)

type RawArguments = Collection RawArgument

type RawSelectionSet = Collection RawSelection

type Spread = (Key, Position)

data RawSelection
  = RawSelectionSet RawArguments
                    RawSelectionSet
                    [Spread]
                    Position
  | RawField RawArguments
             Key
             Position
  deriving (Show)
