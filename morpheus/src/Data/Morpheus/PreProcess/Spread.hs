{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Spread
    ( spreadFieldsWhile
    )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Map                      as M
                                                ( lookup )
import           Data.List                      ( find )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) , Position )
import           Data.Morpheus.Error.Fragment   ( unknownFragment )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , FragmentLib
                                                , Fragment(..)
                                                , GQLQueryRoot(..)
                                                )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , ErrorLocation(..)
                                                )
import qualified Data.Text                     as T

shouldSpread :: [(Text, QuerySelection)] -> Bool
shouldSpread list = case find isFragment list of
    Just _  -> True
    Nothing -> False

isFragment :: (Text, QuerySelection) -> Bool
isFragment (key, Spread _ _) = True
isFragment (key, _         ) = False


validateSpread
    :: GQLQueryRoot
    -> FragmentLib
    -> Position
    -> Text
    -> Validation [(Text, QuerySelection)]
validateSpread root frags location key = case M.lookup key frags of
    Nothing -> Left $ unknownFragment (lineMarks root)  metaData
    Just (Fragment _ _ (SelectionSet _ gqlObj)) -> pure gqlObj
    where metaData = MetaInfo { typeName = "", key = key, position = location}

propagateSpread
    :: GQLQueryRoot
    -> (Text, QuerySelection)
    -> Validation [(Text, QuerySelection)]
propagateSpread root (key, Spread _ location) =
    validateSpread root (fragments root) location key
propagateSpread root (text, value) = pure [(text, value)]

spreadFields :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFields root selectors = concat <$> mapM (propagateSpread root) selectors

spreadFieldsWhile :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFieldsWhile root selectors = spreadFields root selectors >>= checkUpdate
  where
    checkUpdate x = if shouldSpread x then spreadFieldsWhile root x else pure x
