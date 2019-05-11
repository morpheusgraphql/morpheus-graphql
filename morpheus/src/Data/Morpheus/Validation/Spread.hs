module Data.Morpheus.Validation.Spread
  ( getFragment
  , resolveSpread
  ) where

import qualified Data.Map                           as M (lookup)
import           Data.Morpheus.Error.Spread         (cannotBeSpreadOnType, unknownFragment)
import           Data.Morpheus.Types.Error          (Validation)
import           Data.Morpheus.Types.MetaInfo       (Position)
import           Data.Morpheus.Types.Query.Fragment (Fragment (..), FragmentLib, RawFragment)
import           Data.Text                          (Text)
import qualified Data.Text                          as T (concat)

getFragment :: Position -> Text -> FragmentLib -> Validation RawFragment
getFragment position' id' lib =
  case M.lookup id' lib of
    Nothing       -> Left $ unknownFragment id' position'
    Just fragment -> pure fragment

castFragmentType :: Text -> Position -> [Text] -> RawFragment -> Validation RawFragment
castFragmentType key' position' targets' fragment =
  if target fragment `elem` targets'
    then pure fragment
    else Left $ cannotBeSpreadOnType key' (target fragment) position' (T.concat targets')

resolveSpread :: FragmentLib -> [Text] -> Position -> Text -> Validation RawFragment
resolveSpread fragments' allowedTargets' position' key' =
  getFragment position' key' fragments' >>= castFragmentType key' position' allowedTargets'
