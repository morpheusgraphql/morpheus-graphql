{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Fragment
  ( validateFragments
  ) where

import qualified Data.Map                               as M (lookup, toList)
import           Data.Morpheus.Error.Fragment           (cannotBeSpreadOnType, cycleOnFragment,
                                                         fragmentError, unknownFragment)
import           Data.Morpheus.Error.Selection          (selectionError)
import           Data.Morpheus.Error.Utils              (toGQLError)
import           Data.Morpheus.PreProcess.Arguments     (resolveArguments)
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

compareFragmentType :: Meta.MetaInfo -> Type -> Fragment -> Validation Type
compareFragmentType spreadMeta _type fragment =
  if T.name _type == target fragment
    then pure _type
    else Left $ cannotBeSpreadOnType (spreadMeta {Meta.typeName = target fragment}) (T.name _type)

getSpreadType :: FragmentLib -> Type -> Text -> Meta.MetaInfo -> Validation Type
getSpreadType frags _type fragmentID spreadMeta =
  getFragment spreadMeta fragmentID frags >>= compareFragmentType spreadMeta _type

validateFragmentFields :: TypeLib -> GQLQueryRoot -> Type -> (Text, RawSelection) -> Validation Graph
validateFragmentFields typeLib root _parent (name', RawSelectionSet args selectors sPos) = do
  fieldSC <- asSelectionValidation $ fieldOf sPos _parent name'
  typeSC <- asGQLError $ fieldType sPos typeLib fieldSC
  _ <- resolveArguments typeLib root fieldSC sPos args -- TODO do not use heavy validation
  concat <$> mapM (validateFragmentFields typeLib root typeSC) selectors
validateFragmentFields typeLib root _parentType (_name, RawField args _ sPos) = do
  _field <- asSelectionValidation $ fieldOf sPos _parentType _name
  _ <- resolveArguments typeLib root _field sPos args -- TODO do not use heavy validation
  pure []
validateFragmentFields _ root _parent (spreadID, Spread value pos) =
  getSpreadType (fragments root) _parent spreadID spreadMeta >> pure [value]
  where
    spreadMeta = Meta.MetaInfo {Meta.typeName = "", Meta.key = spreadID, Meta.position = pos}

validateFragment :: TypeLib -> GQLQueryRoot -> (Text, Fragment) -> Validation (Text, Graph)
validateFragment lib root (fName, Fragment {content = selection, target = target', position = position'}) = do
  _type <- asGQLError $ existsType (position', fName) target' lib
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
    Just node -> concat <$> mapM checkNode node
    Nothing   -> pure []
  where
    checkNode x =
      if x `elem` history
        then cycleError
        else recurse x
    recurse node = checkForCycle lib node (history ++ [node])
    cycleError = Left $ cycleOnFragment (map (\x -> (x, 0)) history) -- TODO real position
