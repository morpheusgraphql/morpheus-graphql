{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Spread
  ( spreadFieldsWhile
  ) where

import           Data.List                    (find)
import qualified Data.Map                     as M (lookup)
import           Data.Morpheus.Error.Fragment (unknownFragment)
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..), Position)
import           Data.Morpheus.Types.Types    (Fragment (..), FragmentLib, GQLQueryRoot (..),
                                               QuerySelection (..), SelectionSet, Validation)
import           Data.Text                    (Text)

shouldSpread :: [(Text, QuerySelection)] -> Bool
shouldSpread list =
  case find isFragment list of
    Just _  -> True
    Nothing -> False

isFragment :: (Text, QuerySelection) -> Bool
isFragment (_key, Spread _ _) = True
isFragment (_key, _)          = False

validateSpread :: GQLQueryRoot -> FragmentLib -> Position -> Text -> Validation [(Text, QuerySelection)]
validateSpread _root frags location spreadID =
  case M.lookup spreadID frags of
    Nothing                                       -> Left $ unknownFragment metaData
    Just (Fragment _ _ (SelectionSet _ gqlObj _)) -> pure gqlObj
  where
    metaData = MetaInfo {typeName = "", key = spreadID, position = location}

propagateSpread :: GQLQueryRoot -> (Text, QuerySelection) -> Validation [(Text, QuerySelection)]
propagateSpread root (spreadID, Spread _ location) = validateSpread root (fragments root) location spreadID
propagateSpread _ (text, value) = pure [(text, value)]

spreadFields :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFields root selectors = concat <$> mapM (propagateSpread root) selectors

spreadFieldsWhile :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFieldsWhile root selectors = spreadFields root selectors >>= checkUpdate
  where
    checkUpdate x =
      if shouldSpread x
        then spreadFieldsWhile root x
        else pure x
