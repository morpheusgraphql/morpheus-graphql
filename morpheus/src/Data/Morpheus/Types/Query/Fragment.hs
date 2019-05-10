module Data.Morpheus.Types.Query.Fragment
  ( Fragment(..)
  , FragmentLib
  , RawFragment
  ) where

import           Data.Map                               (Map)
import           Data.Morpheus.Types.Core               (Key)
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.RawSelection (RawSelectionSet)

data Fragment a = Fragment
  { key      :: Key
  , target   :: Key
  , position :: Position
  , content  :: a
  } deriving (Show)

type RawFragment = Fragment RawSelectionSet

type FragmentLib = Map Key RawFragment
