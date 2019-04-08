module Data.Morpheus.PreProcess.Spread
  ( prepareRawSelection
  , getFragment
  , castFragmentType
  ) where

import qualified Data.Map                               as M (lookup)
import           Data.Morpheus.Error.Selection          (selectionError)
import           Data.Morpheus.Error.Spread             (cannotBeSpreadOnType, unknownFragment)
import           Data.Morpheus.Error.Utils              (toGQLError)
import           Data.Morpheus.PreProcess.Arguments     (onlyResolveArguments)
import           Data.Morpheus.PreProcess.Utils         (existsObjectType, fieldOf)
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), ObjectField (..), OutputObject,
                                                         TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.Types    as SC (Field (..))
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..), Position)
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..))
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

getFragment :: Position -> Text -> FragmentLib -> Validation Fragment
getFragment position' id' lib =
  case M.lookup id' lib of
    Nothing       -> Left $ unknownFragment id' position'
    Just fragment -> pure fragment

castFragmentType :: MetaInfo -> GObject ObjectField -> Fragment -> Validation Fragment
castFragmentType spreadMeta (GObject _ core) fragment =
  if name core == target fragment
    then pure fragment
    else Left $ cannotBeSpreadOnType (spreadMeta {typeName = target fragment}) (name core)

selectionSetFromSpread :: TypeLib -> GQLQueryRoot -> GObject ObjectField -> Position -> Text -> Validation SelectionSet
selectionSetFromSpread lib' root' parentType' position' id' =
  getFragment position' id' (fragments root') >>= cast >>= replace
  where
    cast fragment' =
      castFragmentType
        (MetaInfo {Meta.position = position', Meta.key = id', Meta.typeName = target fragment'})
        parentType'
        fragment'
    replace fragment = concat <$> mapM (replaceVariableAndSpread lib' root' parentType') (content fragment)

asSelectionValidation :: MetaValidation a -> Validation a
asSelectionValidation = toGQLError selectionError

replaceVariableAndSpread ::
     TypeLib -> GQLQueryRoot -> GObject ObjectField -> (Text, RawSelection) -> Validation SelectionSet
replaceVariableAndSpread lib' root' (GObject parentFields core) (key', RawSelectionSet rawArgs rawSelectors sPos) = do
  field' <- asSelectionValidation $ fieldOf (sPos, name core) parentFields key'
  fieldType' <- asSelectionValidation $ existsObjectType (sPos, name core) (SC.fieldType $ fieldContent field') lib'
  args' <- onlyResolveArguments root' sPos rawArgs
  sel <- concat <$> mapM (replaceVariableAndSpread lib' root' fieldType') rawSelectors
  pure [(key', SelectionSet args' sel sPos)]
replaceVariableAndSpread _ root' _ (sKey, RawField rawArgs field sPos) = do
  args' <- onlyResolveArguments root' sPos rawArgs
  pure [(sKey, Field args' field sPos)]
replaceVariableAndSpread lib' root' parent' (spreadID, Spread _ sPos) =
  selectionSetFromSpread lib' root' parent' sPos spreadID

prepareRawSelection :: TypeLib -> GQLQueryRoot -> RawSelectionSet -> OutputObject -> Validation SelectionSet
prepareRawSelection lib' root' sel query' = concat <$> mapM (replaceVariableAndSpread lib' root' query') sel
