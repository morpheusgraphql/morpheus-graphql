module Data.Morpheus.Types.Query.Fragment
  ( Fragment(..)
  , FragmentLib
  ) where

import           Data.Map                            (Map)
import           Data.Morpheus.Types.Core            (Key)
import           Data.Morpheus.Types.MetaInfo        (Position)
import           Data.Morpheus.Types.Query.Selection (SelectionSet)

data Fragment = Fragment
  { key     :: Key
  , target  :: Key
  , pos     :: Position
  , content :: SelectionSet
  } deriving (Show)

type FragmentLib = Map Key Fragment
