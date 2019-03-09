{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Fragment
    ( validateFragments
    )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Map                      as M
                                                ( lookup
                                                , toList
                                                )
import           Data.List                      ( find )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Morpheus.Error.Fragment   ( unknownFragment
                                                , unsupportedSpreadOnType
                                                , cycleOnFragment
                                                )
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
                                                , existsType
                                                )
import           Data.Morpheus.PreProcess.Arguments
                                                ( validateArguments )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T


type Graph = [Text];
type RootGraph = [(Text, Graph)];

getFragment :: MetaInfo -> Text -> FragmentLib -> Validation Fragment
getFragment meta key lib = case M.lookup key lib of
    Nothing       -> Left $ unknownFragment meta
    Just fragment -> pure fragment

compareFragmentType
    :: MetaInfo -> MetaInfo -> GQL__Type -> Fragment -> Validation GQL__Type
compareFragmentType parent child _type fragment =
    if (T.name _type == target fragment)
        then pure _type
        else Left $ unsupportedSpreadOnType parent child

getSpreadType :: FragmentLib -> GQL__Type -> Text -> Validation GQL__Type
getSpreadType frags _type key = getFragment (spread "") key frags >>= fragment
  where
    fragment fg = compareFragmentType parent (spread $ target fg) _type fg
    parent = MetaInfo { className = T.name _type, cons = "", key = "" }
    spread typeName = MetaInfo { className = typeName, cons = "", key = key }


validateFragmentFields
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Validation Graph
validateFragmentFields typeLib root _parent (_name, SelectionSet head selectors)
    = do
        _type  <- typeBy typeLib _parent _name
        _field <- fieldOf _parent _name
        head'  <- validateArguments typeLib root _field head
        concat <$> mapM (validateFragmentFields typeLib root _type) selectors

validateFragmentFields typeLib root _parentType (_name, Field head field) = do
    _field <- fieldOf _parentType _name
    head'  <- validateArguments typeLib root _field head
    pure []

validateFragmentFields lib root _parent (key, Spread value _) =
    getSpreadType (fragments root) _parent key >> pure [value]

validateFragmentFields _ _ _ _ = pure []


validateFragment
    :: GQLTypeLib
    -> GQLQueryRoot
    -> (Text, Fragment)
    -> Validation (Text, Graph)
validateFragment lib root (fName, frag) = do
    _type <- existsType (target frag) lib
    let (SelectionSet _ selection) = (fragmentContent frag)
    fragmentLinks <-
        concat <$> mapM (validateFragmentFields lib root _type) selection
    pure (fName, fragmentLinks)


validateFragments :: GQLTypeLib -> GQLQueryRoot -> Validation GQLQueryRoot
validateFragments lib root = do
    val <-
        mapM (validateFragment lib root) (M.toList $ fragments root)
            >>= detectLoopOnFragments
    pure root


detectLoopOnFragments :: RootGraph -> Validation RootGraph
detectLoopOnFragments lib = concat <$> mapM checkFragment lib
    where checkFragment (key, _) = checkForCycle lib key [key]


checkForCycle :: RootGraph -> Text -> [Text] -> Validation RootGraph
checkForCycle lib parentNode history = case lookup parentNode lib of
    Just nodes -> concat <$> mapM checkNode nodes
    Nothing    -> pure []
  where
    checkNode x = if elem x history then error x else recurse x
    recurse node = checkForCycle lib node (history ++ [node])
    error x = Left $ cycleOnFragment history

