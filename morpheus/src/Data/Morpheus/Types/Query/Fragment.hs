module Data.Morpheus.Types.Query.Fragment
  ( Fragment(..)
  , FragmentLib
  ) where

import           Data.Map                               (Map)
import           Data.Morpheus.Types.Core               (Key)
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.RawSelection (RawSelectionSet)

data Fragment = Fragment
  { key     :: Key
  , target  :: Key
  , pos     :: Position
  , content :: RawSelectionSet
  } deriving (Show)

type FragmentLib = Map Key Fragment
