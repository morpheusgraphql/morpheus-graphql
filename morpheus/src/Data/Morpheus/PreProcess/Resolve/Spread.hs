module Data.Morpheus.PreProcess.Resolve.Spread
  ( getFragment
  , castFragmentType
  , resolveSpread
  ) where

import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Spread             (cannotBeSpreadOnType, unknownFragment)
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), ObjectField (..))
import           Data.Morpheus.Types.Error              (GQLErrors, Validation)
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection (RawSelectionSet)
import           Data.Text                              (Text)

getFragment :: Position -> Text -> FragmentLib -> Validation Fragment
getFragment position' id' lib =
  case M.lookup id' lib of
    Nothing       -> Left $ unknownFragment id' position'
    Just fragment -> pure fragment

castFragmentType :: Text -> Position -> GObject ObjectField -> Fragment -> Validation Fragment
castFragmentType key' position' (GObject _ core) fragment =
  if name core == target fragment
    then pure fragment
    else Left $ cannotBeSpreadOnType key' (target fragment) position' (name core)

resolveSpread :: FragmentLib -> GObject ObjectField -> Position -> Text -> Validation RawSelectionSet
resolveSpread fragments' parentType' position' key' = content <$> (getFragment position' key' fragments' >>= cast)
  where
    cast = castFragmentType key' position' parentType'
