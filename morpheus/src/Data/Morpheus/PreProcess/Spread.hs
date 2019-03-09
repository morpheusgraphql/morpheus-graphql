{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Spread
    ( spreadFieldsWhile
    )
where

import           Data.Text                      ( Text )
import qualified Data.Map                      as M
                                                ( lookup )
import           Data.List                      ( find )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Morpheus.ErrorMessage     ( unknownFragment )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , FragmentLib
                                                , Fragment(..)
                                                , GQLQueryRoot(..)
                                                )

shouldSpread :: [(Text, QuerySelection)] -> Bool
shouldSpread list = case find isFragment list of
    Just _  -> True
    Nothing -> False

isFragment :: (Text, QuerySelection) -> Bool
isFragment (key, Spread _ _) = True
isFragment (key, _         ) = False

validateSpread :: FragmentLib -> Text -> Validation [(Text, QuerySelection)]
validateSpread frags key = case M.lookup key frags of
    Nothing -> Left $ unknownFragment $ MetaInfo
        { className = ""
        , cons      = ""
        , key       = key
        }
    Just (Fragment _ _ (SelectionSet _ gqlObj)) -> pure gqlObj

propagateSpread
    :: GQLQueryRoot
    -> (Text, QuerySelection)
    -> Validation [(Text, QuerySelection)]
propagateSpread root (key , Spread _ _) = validateSpread (fragments root) key
propagateSpread root (text, value     ) = pure [(text, value)]

spreadFields :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFields root selectors = concat <$> mapM (propagateSpread root) selectors

spreadFieldsWhile :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFieldsWhile root selectors = spreadFields root selectors >>= checkUpdate
  where
    checkUpdate x = if shouldSpread x then spreadFieldsWhile root x else pure x
