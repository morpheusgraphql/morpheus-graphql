module Data.Morpheus.Validation.Spread
  ( getFragment
  , resolveSpread
  , castFragmentType
  ) where

import qualified Data.Map                                      as M (lookup)
import           Data.Morpheus.Error.Spread                    (cannotBeSpreadOnType, unknownFragment)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), FragmentLib, Reference (..))
import           Data.Morpheus.Types.Internal.Base             (Position)
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T (concat)

getFragment :: Position -> Text -> FragmentLib -> Validation Fragment
getFragment position' id' lib =
  case M.lookup id' lib of
    Nothing       -> Left $ unknownFragment id' position'
    Just fragment -> pure fragment

castFragmentType :: Maybe Text -> Position -> [Text] -> Fragment -> Validation Fragment
castFragmentType key' position' targets' fragment@Fragment {fragmentType = type'} =
  if type' `elem` targets'
    then pure fragment
    else Left $ cannotBeSpreadOnType key' type' position' (T.concat targets')

resolveSpread :: FragmentLib -> [Text] -> Reference -> Validation Fragment
resolveSpread fragments' allowedTargets' Reference {referenceName = key', referencePosition = position'} =
  getFragment position' key' fragments' >>= castFragmentType (Just key') position' allowedTargets'
