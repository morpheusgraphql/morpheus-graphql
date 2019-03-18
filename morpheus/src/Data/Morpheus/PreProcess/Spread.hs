{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Spread
  ( spreadFields
  ) where

import           Data.List                              (find)
import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Fragment           (unknownFragment)
import           Data.Morpheus.Types.Error              (Validation)
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..), Position)
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection (RawSelection (..), RawSelectionSet)
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

validateSpread :: GQLQueryRoot -> FragmentLib -> Meta.Position -> Text -> Validation RawSelectionSet
validateSpread _root frags location spreadID =
  case M.lookup spreadID frags of
    Nothing                             -> Left $ unknownFragment metaData
    Just Fragment {content = selection} -> pure selection
  where
    metaData = Meta.MetaInfo {Meta.typeName = "", Meta.key = spreadID, Meta.position = location}

propagateSpread :: GQLQueryRoot -> (Text, RawSelection) -> Validation RawSelectionSet
propagateSpread root (spreadID, Spread _ location) =
  validateSpread root (fragments root) location spreadID >>= checkUpdate
  where
    checkUpdate x =
      if shouldSpread x
        then spreadFields root x
        else pure x
propagateSpread _ value = pure [value]

spreadFields :: GQLQueryRoot -> RawSelectionSet -> Validation RawSelectionSet
spreadFields root selectors = concat <$> mapM (propagateSpread root) selectors
