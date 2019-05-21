module Data.Morpheus.Types.Internal.AST.Fragment
  ( Fragment(..)
  , FragmentLib
  ) where

import           Data.Map                                      (Map)
import           Data.Morpheus.Types.Internal.AST.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Internal.Base             (Key, Position)

data Fragment = Fragment
  { key      :: Key
  , target   :: Key
  , position :: Position
  , content  :: RawSelectionSet
  } deriving (Show)

type FragmentLib = Map Key Fragment
