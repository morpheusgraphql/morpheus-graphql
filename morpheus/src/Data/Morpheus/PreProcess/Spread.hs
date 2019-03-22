{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Spread
  ( prepareRawSelection
  ) where

import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Fragment           (unknownFragment)
import           Data.Morpheus.PreProcess.Arguments     (onlyResolveArguments)
import           Data.Morpheus.Types.Error              (Validation)
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..), Position)
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..))
import           Data.Morpheus.Types.Query.RawSelection (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

-- TODO :: add on type validation as in fragment
selectionSetFromSpread :: GQLQueryRoot -> Meta.Position -> Text -> Validation SelectionSet
selectionSetFromSpread _root location spreadID =
  case M.lookup spreadID (fragments _root) of
    Nothing -> Left $ unknownFragment metaData
    Just Fragment {content = selection} -> concat <$> mapM (replaceVariableAndSpread _root) selection
  where
    metaData = Meta.MetaInfo {Meta.typeName = "", Meta.key = spreadID, Meta.position = location}

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
