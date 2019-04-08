module Data.Morpheus.PreProcess.Spread
  ( prepareRawSelection
  , getFragment
  ) where

import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Spread             (unknownFragment)
import           Data.Morpheus.PreProcess.Arguments     (onlyResolveArguments)
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

getFragment :: Position -> Text -> FragmentLib -> Validation Fragment
getFragment position' id' lib =
  case M.lookup id' lib of
    Nothing       -> Left $ unknownFragment id' position'
    Just fragment -> pure fragment

-- TODO :: add on type validation as in fragment
selectionSetFromSpread :: GQLQueryRoot -> Position -> Text -> Validation SelectionSet
selectionSetFromSpread _root position' id' = getFragment position' id' (fragments _root) >>= replace
  where
    replace fragment = concat <$> mapM (replaceVariableAndSpread _root) (content fragment)

replaceVariableAndSpread :: GQLQueryRoot -> (Text, RawSelection) -> Validation SelectionSet
replaceVariableAndSpread root (sKey, RawSelectionSet rawArgs rawSelectors sPos) = do
  sel <- concat <$> mapM (replaceVariableAndSpread root) rawSelectors
  args <- onlyResolveArguments root sPos rawArgs
  pure [(sKey, SelectionSet args sel sPos)]
replaceVariableAndSpread root (sKey, RawField rawArgs field sPos) = do
  args <- onlyResolveArguments root sPos rawArgs
  pure [(sKey, Field args field sPos)]
replaceVariableAndSpread root (spreadID, Spread _ sPos) = selectionSetFromSpread root sPos spreadID

prepareRawSelection :: GQLQueryRoot -> RawSelectionSet -> Validation SelectionSet
prepareRawSelection root sel = concat <$> mapM (replaceVariableAndSpread root) sel
