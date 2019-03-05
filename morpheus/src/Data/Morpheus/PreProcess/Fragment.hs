{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Fragment
    ( validateFragments
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



validateSpread :: FragmentLib -> Text -> Validation [(Text, QuerySelection)]
validateSpread frags key = case M.lookup key frags of
    Nothing -> Left $ unknownFragment $ MetaInfo
        { className = ""
        , cons      = ""
        , key       = key
        }
    Just (Fragment _ _ (SelectionSet _ gqlObj)) -> pure gqlObj

validateFragmentField
    :: GQLQueryRoot
    -> (Text, QuerySelection)
    -> Validation [(Text, QuerySelection)]
validateFragmentField root (key, Spread _) =
    validateSpread (fragments root) key
validateFragmentField root (text, value) = pure [(text, value)]

validateFragmentFields
    :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
validateFragmentFields root selectors =
    concat <$> mapM (validateFragmentField root) selectors

validateFragments :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
validateFragments = validateFragmentFields

