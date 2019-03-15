{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Fragment
  ( validateFragments
  ) where

import qualified Data.Map                           as M (lookup, toList)
import           Data.Morpheus.Error.Fragment       (cycleOnFragment, fragmentError,
                                                     unknownFragment, unsupportedSpreadOnType)
import           Data.Morpheus.PreProcess.Arguments (validateArguments)
import           Data.Morpheus.PreProcess.Utils     (existsType, fieldOf, typeBy)
import qualified Data.Morpheus.Schema.GQL__Type     as T
import           Data.Morpheus.Types.Error          (MetaValidation)
import           Data.Morpheus.Types.Introspection  (GQLTypeLib, GQL__Type)
import           Data.Morpheus.Types.MetaInfo       (MetaInfo (..))
import           Data.Morpheus.Types.Types          (Fragment (..), FragmentLib, GQLQueryRoot (..),
                                                     QuerySelection (..), Validation)
import           Data.Text                          (Text)

type Graph = [Text]

type RootGraph = [(Text, Graph)]

asGQLError :: MetaValidation a -> Validation a
asGQLError (Left err)    = Left $ fragmentError err
asGQLError (Right value) = pure value

getFragment :: MetaInfo -> Text -> FragmentLib -> Validation Fragment
getFragment meta fragmentID lib =
  case M.lookup fragmentID lib of
    Nothing       -> Left $ unknownFragment meta
    Just fragment -> pure fragment

compareFragmentType :: MetaInfo -> MetaInfo -> GQL__Type -> Fragment -> Validation GQL__Type
compareFragmentType parent child _type fragment =
  if T.name _type == target fragment
    then pure _type
    else Left $ unsupportedSpreadOnType parent child

getSpreadType :: FragmentLib -> GQL__Type -> Text -> Validation GQL__Type
getSpreadType frags _type fragmentID = getFragment (spread "") fragmentID frags >>= fragment
  where
    fragment fg = compareFragmentType parent (spread $ target fg) _type fg
    parent = MetaInfo {typeName = T.name _type, key = "", position = 0}
    spread name = MetaInfo {typeName = name, key = fragmentID, position = 0}

validateFragmentFields :: GQLTypeLib -> GQLQueryRoot -> GQL__Type -> (Text, QuerySelection) -> Validation Graph
validateFragmentFields typeLib root _parent (_name, SelectionSet args selectors pos) = do
  _type <- asGQLError $ typeBy pos typeLib _parent _name
  _field <- fieldOf pos _parent _name
  _ <- validateArguments typeLib root _field args
  concat <$> mapM (validateFragmentFields typeLib root _type) selectors
validateFragmentFields typeLib root _parentType (_name, Field args _ pos) = do
  _field <- fieldOf pos _parentType _name
  _ <- validateArguments typeLib root _field args
  pure []
validateFragmentFields _ root _parent (spreadID, Spread value _) =
  getSpreadType (fragments root) _parent spreadID >> pure [value]
validateFragmentFields _ _ _ _ = pure []

validateFragment :: GQLTypeLib -> GQLQueryRoot -> (Text, Fragment) -> Validation (Text, Graph)
validateFragment lib root (fName, frag) = do
  _type <- asGQLError $ existsType (target frag) lib
  let (SelectionSet _ selection _pos) = fragmentContent frag
  fragmentLinks <- concat <$> mapM (validateFragmentFields lib root _type) selection
  pure (fName, fragmentLinks)

validateFragments :: GQLTypeLib -> GQLQueryRoot -> Validation GQLQueryRoot
validateFragments lib root = do
  _ <- mapM (validateFragment lib root) (M.toList $ fragments root) >>= detectLoopOnFragments
  pure root

detectLoopOnFragments :: RootGraph -> Validation RootGraph
detectLoopOnFragments lib = concat <$> mapM checkFragment lib
  where
    checkFragment (fragmentID, _) = checkForCycle lib fragmentID [fragmentID]

checkForCycle :: RootGraph -> Text -> [Text] -> Validation RootGraph
checkForCycle lib parentNode history =
  case lookup parentNode lib of
    Just nodes -> concat <$> mapM checkNode nodes
    Nothing    -> pure []
  where
    checkNode x =
      if x `elem` history
        then cycleError
        else recurse x
    recurse node = checkForCycle lib node (history ++ [node])
    cycleError = Left $ cycleOnFragment history
