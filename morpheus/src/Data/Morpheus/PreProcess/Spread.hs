{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Spread
  -- spreadFields,
  ( resolveSpread
  ) where

import           Data.List                              (find)
import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Fragment           (unknownFragment)
import           Data.Morpheus.Types.Error              (Validation)
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..), Position)
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..))
import           Data.Morpheus.Types.Query.RawSelection (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

shouldSpread :: RawSelectionSet -> Bool
shouldSpread list =
  case find isFragment list of
    Just _  -> True
    Nothing -> False

isFragment :: (Text, RawSelection) -> Bool
isFragment (_key, Spread _ _) = True
isFragment (_key, _)          = False

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
convertTo root (key_, RawSelectionSet _ selectors pos_) = do
  list <- mapM (convertTo root) selectors
  pure [(key_, SelectionSet [] (concat list) pos_)]
convertTo root (spreadID, Spread _ location) = validateSpread root location spreadID
convertTo _ (sName, RawField _ field pos_) = pure [(sName, Field [] field pos_)]

resolveSpread :: GQLQueryRoot -> RawSelectionSet -> Validation SelectionSet
resolveSpread root sel = concat <$> mapM (convertTo root) sel
