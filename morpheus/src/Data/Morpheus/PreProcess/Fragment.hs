{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Fragment
  ( validateFragments
  ) where

import qualified Data.Map                               as M (lookup, toList)
import           Data.Morpheus.Error.Fragment           (cycleOnFragment, fragmentError,
                                                         unknownFragment, unsupportedSpreadOnType)
import           Data.Morpheus.Error.Selection          (selectionError)
import           Data.Morpheus.Error.Utils              (toGQLError)
import           Data.Morpheus.PreProcess.Arguments     (validateArguments)
import           Data.Morpheus.PreProcess.Utils         (existsType, fieldOf, fieldType)
import qualified Data.Morpheus.Schema.Type              as T (name)
import           Data.Morpheus.Schema.Utils.Utils       (Type, TypeLib)
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..))
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection (RawSelection (..))
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

type Graph = [Text]

type RootGraph = [(Text, Graph)]

asSelectionValidation :: MetaValidation a -> Validation a
asSelectionValidation = toGQLError selectionError

asGQLError :: MetaValidation a -> Validation a
asGQLError (Left err)    = Left $ fragmentError err
asGQLError (Right value) = pure value

getFragment :: Meta.MetaInfo -> Text -> FragmentLib -> Validation Fragment
getFragment meta fragmentID lib =
  case M.lookup fragmentID lib of
    Nothing       -> Left $ unknownFragment meta
    Just fragment -> pure fragment

compareFragmentType :: Meta.MetaInfo -> Meta.MetaInfo -> Type -> Fragment -> Validation Type
compareFragmentType parent child _type fragment =
  if T.name _type == target fragment
    then pure _type
    else Left $ unsupportedSpreadOnType parent child

getSpreadType :: FragmentLib -> Type -> Text -> Validation Type
getSpreadType frags _type fragmentID = getFragment (spread "") fragmentID frags >>= fragment
  where
    fragment fg = compareFragmentType parent (spread $ target fg) _type fg
    parent = Meta.MetaInfo {Meta.typeName = T.name _type, Meta.key = "", Meta.position = 0}
    spread name = Meta.MetaInfo {Meta.typeName = name, Meta.key = fragmentID, Meta.position = 0}

validateFragmentFields :: TypeLib -> GQLQueryRoot -> Type -> (Text,RawSelection) -> Validation Graph
validateFragmentFields typeLib root _parent (name', RawSelectionSet args selectors pos_) = do
  fieldSC <- asSelectionValidation $ fieldOf pos_ _parent name'
  typeSC <- asGQLError $ fieldType pos_ typeLib fieldSC
  _ <- validateArguments typeLib root fieldSC args
  concat <$> mapM (validateFragmentFields typeLib root typeSC) selectors
validateFragmentFields typeLib root _parentType (_name, RawField args _ pos_) = do
  _field <- asSelectionValidation $ fieldOf pos_ _parentType _name
  _ <- validateArguments typeLib root _field args
  pure []
validateFragmentFields _ root _parent (spreadID, Spread value _) =
  getSpreadType (fragments root) _parent spreadID >> pure [value]

validateFragment :: TypeLib -> GQLQueryRoot -> (Text, Fragment) -> Validation (Text, Graph)
validateFragment lib root (fName, Fragment {content = selection, target = target'}) = do
  _type <- asGQLError $ existsType target' lib
  fragmentLinks <- concat <$> mapM (validateFragmentFields lib root _type) selection
  pure (fName, fragmentLinks)

validateFragments :: TypeLib -> GQLQueryRoot -> Validation ()
validateFragments lib root = mapM (validateFragment lib root) (M.toList $ fragments root) >>= detectLoopOnFragments

detectLoopOnFragments :: RootGraph -> Validation ()
detectLoopOnFragments lib = mapM_ checkFragment lib
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
