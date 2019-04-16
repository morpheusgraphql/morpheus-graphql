module Data.Morpheus.PreProcess.Resolve.Spread
  ( getFragment
  , castFragmentType
  , resolveSpread
  ) where

import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Spread             (cannotBeSpreadOnType, unknownFragment)
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), ObjectField (..))
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..), Position)
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..))
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection (RawSelectionSet)
import           Data.Text                              (Text)

getFragment :: Position -> Text -> FragmentLib -> Validation Fragment
getFragment position' id' lib =
  case M.lookup id' lib of
    Nothing       -> Left $ unknownFragment id' position'
    Just fragment -> pure fragment

castFragmentType :: MetaInfo -> GObject ObjectField -> Fragment -> Validation Fragment
castFragmentType spreadMeta (GObject _ core) fragment =
  if name core == target fragment
    then pure fragment
    else Left $ cannotBeSpreadOnType (spreadMeta {typeName = target fragment}) (name core)

resolveSpread :: FragmentLib -> GObject ObjectField -> Position -> Text -> Validation RawSelectionSet
resolveSpread fragments' parentType' position' id' = content <$> (getFragment position' id' fragments' >>= cast)
  where
    cast fragment' =
      castFragmentType
        (MetaInfo {Meta.position = position', Meta.key = id', Meta.typeName = target fragment'})
        parentType'
        fragment'
