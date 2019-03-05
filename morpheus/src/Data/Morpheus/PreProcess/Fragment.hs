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
import           Data.Morpheus.Types.Introspection
                                                ( GQLTypeLib
                                                , GQL__Type
                                                )
import           Data.Morpheus.PreProcess.Utils ( typeBy
                                                , fieldOf
                                                )
import           Data.Morpheus.PreProcess.Arguments
                                                ( validateArguments )

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

--validateFragmentFields
--    :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
--validateFragmentFields root selectors =
--    concat <$> mapM (validateFragmentField root) selectors

validateFragments
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Validation (Text, QuerySelection)
validateFragments = validateFragmentFields


validateFragmentFields
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Validation (Text, QuerySelection)
validateFragmentFields typeLib root _parent (_name, SelectionSet head selectors)
    = do
        _type  <- typeBy typeLib _parent _name
        _field <- fieldOf _parent _name
        head'  <- validateArguments typeLib root _field head
       -- selectors' <-
        --    concat
        --        <$> mapM (validateFragmentFields typeLib root _field) selectors
        pure (_name, SelectionSet head' selectors)

validateFragmentFields typeLib root _parentType (_name, Field head field) = do
    _field <- fieldOf _parentType _name
    head'  <- validateArguments typeLib root _field head
    pure (_name, Field head' field)

validateFragmentFields _ _ _ x = pure x

