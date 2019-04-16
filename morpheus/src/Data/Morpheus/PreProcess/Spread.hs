module Data.Morpheus.PreProcess.Spread
  ( prepareRawSelection
  , getFragment
  , castFragmentType
  ) where

import qualified Data.Map                                    as M (lookup)
import           Data.Morpheus.Error.Spread                  (cannotBeSpreadOnType, unknownFragment)
import           Data.Morpheus.PreProcess.Selection          (lookupSelectionObjectFieldType)
import           Data.Morpheus.PreProcess.Validate.Arguments (onlyResolveArguments)
import           Data.Morpheus.Schema.Internal.Types         (Core (..), GObject (..), ObjectField (..), OutputObject,
                                                              TypeLib (..))
import           Data.Morpheus.Types.Error                   (Validation)
import           Data.Morpheus.Types.MetaInfo                (MetaInfo (..), Position)
import qualified Data.Morpheus.Types.MetaInfo                as Meta (MetaInfo (..))
import           Data.Morpheus.Types.Query.Fragment          (Fragment (..), FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection      (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection         (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types                   (Variables)
import           Data.Text                                   (Text)

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

selectionSetFromSpread ::
     TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> Position -> Text -> Validation SelectionSet
selectionSetFromSpread lib' fragments' variables' parentType' position' id' =
  getFragment position' id' fragments' >>= cast >>= replace
  where
    cast fragment' =
      castFragmentType
        (MetaInfo {Meta.position = position', Meta.key = id', Meta.typeName = target fragment'})
        parentType'
        fragment'
    replace fragment =
      concat <$> mapM (replaceVariableAndSpread lib' fragments' variables' parentType') (content fragment)
replaceVariableAndSpread ::
     TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> (Text, RawSelection) -> Validation SelectionSet
replaceVariableAndSpread lib' fragments' variables' parent' (key', RawSelectionSet rawArgs rawSelectors position') = do
  fieldType' <- lookupSelectionObjectFieldType position' key' lib' parent'
  args' <- onlyResolveArguments variables' position' rawArgs
  sel <- concat <$> mapM (replaceVariableAndSpread lib' fragments' variables' fieldType') rawSelectors
  pure [(key', SelectionSet args' sel position')]
replaceVariableAndSpread _ _ variables' _ (sKey, RawField rawArgs field sPos) = do
  args' <- onlyResolveArguments variables' sPos rawArgs
  pure [(sKey, Field args' field sPos)]
replaceVariableAndSpread lib' fragments' variables' parent' (spreadID, Spread _ sPos) =
  selectionSetFromSpread lib' fragments' variables' parent' sPos spreadID

prepareRawSelection :: TypeLib -> FragmentLib -> Variables -> RawSelectionSet -> OutputObject -> Validation SelectionSet
prepareRawSelection lib' fragments' variables' sel query' =
  concat <$> mapM (replaceVariableAndSpread lib' fragments' variables' query') sel
