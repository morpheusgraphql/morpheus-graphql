module Data.Morpheus.Types.Query.RawSelection
  ( RawArgument(..)
  , RawArguments
  , RawSelectionSet
  , RawSelection(..)
  ) where

import           Data.Morpheus.Types.Core           (Collection, Key)
import           Data.Morpheus.Types.Internal.Value (Value)
import           Data.Morpheus.Types.MetaInfo       (Position)

data RawArgument
  = VariableReference Key
                      Position
  | Argument Value
             Position
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
  | InlineFragment Key
                   RawSelectionSet
                   Position
  | Spread Key
           Position
  deriving (Show)
