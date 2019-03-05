{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Fragment
    ( validateFragments
    )
where

import           Data.Text                      ( Text )
import qualified Data.Map                      as M
                                                ( lookup
                                                , toList
                                                )
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
                                                , existsType
                                                )
import           Data.Morpheus.PreProcess.Arguments
                                                ( validateArguments )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T

getSpreadType :: FragmentLib -> GQL__Type -> Text -> Validation GQL__Type
getSpreadType frags _type key = case M.lookup key frags of
    Nothing -> Left $ unknownFragment $ MetaInfo
        { className = ""
        , cons      = ""
        , key       = key
        }
    Just fragment -> if (T.name _type == target fragment)
        then pure _type
        else Left $ unknownFragment $ MetaInfo
            { className = ""
            , cons = ""
            , key = "spread 'TODO:name' with type '' couldnot spread on type"
            }


validateFragmentFields
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Validation (Text, QuerySelection)
validateFragmentFields typeLib root _parent (_name, SelectionSet head selectors)
    = do
        _type      <- typeBy typeLib _parent _name
        _field     <- fieldOf _parent _name
        head'      <- validateArguments typeLib root _field head
        selectors' <- mapM (validateFragmentFields typeLib root _type) selectors
        pure (_name, SelectionSet head' selectors)

validateFragmentFields typeLib root _parentType (_name, Field head field) = do
    _field <- fieldOf _parentType _name
    head'  <- validateArguments typeLib root _field head
    pure (_name, Field head' field)

validateFragmentFields lib root _parent (key, Spread value) =
    getSpreadType (fragments root) _parent key >> pure (key, Spread value)

validateFragmentFields _ _ _ x = pure x


validateFragment
    :: GQLTypeLib
    -> GQLQueryRoot
    -> (Text, Fragment)
    -> Validation [(Text, QuerySelection)]
validateFragment lib root (fName, frag) = do
    _type <- existsType (target frag) lib
    let (SelectionSet _ selection) = (fragmentContent frag)
    mapM (validateFragmentFields lib root _type) selection


validateFragments :: GQLTypeLib -> GQLQueryRoot -> Validation GQLQueryRoot
validateFragments lib root = do
    val <- mapM (validateFragment lib root) (M.toList $ fragments root)
    pure root


