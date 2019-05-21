module Data.Morpheus.Types.Internal.AST.Fragment
  ( Fragment(..)
  , FragmentLib
  ) where

import           Data.Map                                      (Map)
import           Data.Morpheus.Types.Internal.AST.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Internal.Base             (Key, Position)

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show)

type FragmentLib = Map Key Fragment
