{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Spread
  ( resolveSpread
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

validateSpread :: GQLQueryRoot -> Meta.Position -> Text -> Validation SelectionSet
validateSpread _root location spreadID =
  case M.lookup spreadID (fragments _root) of
    Nothing -> Left $ unknownFragment metaData
    Just Fragment {content = selection} -> do
      list <- mapM (convertTo _root) selection
      pure (concat list)
  where
    metaData = Meta.MetaInfo {Meta.typeName = "", Meta.key = spreadID, Meta.position = location}

convertTo :: GQLQueryRoot -> (Text, RawSelection) -> Validation SelectionSet
convertTo root (sKey, RawSelectionSet rawArgs rawSelectors sPos) = do
  sel <- concat <$> mapM (convertTo root) rawSelectors
  args <- onlyResolveArguments root rawArgs
  pure [(sKey, SelectionSet args sel sPos)]
convertTo root (sKey, RawField rawArgs field sPos) = do
  args <- onlyResolveArguments root rawArgs
  pure [(sKey, Field args field sPos)]
convertTo root (spreadID, Spread _ sPos) = validateSpread root sPos spreadID

resolveSpread :: GQLQueryRoot -> RawSelectionSet -> Validation SelectionSet
resolveSpread root sel = concat <$> mapM (convertTo root) sel
