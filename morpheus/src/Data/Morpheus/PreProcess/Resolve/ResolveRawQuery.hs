module Data.Morpheus.PreProcess.Resolve.ResolveRawQuery
  ( resolveRawQuery
  ) where

import           Data.Morpheus.Error.Selection              (hasNoSubfields)
import           Data.Morpheus.PreProcess.Resolve.Arguments (resolveArguments)
import           Data.Morpheus.PreProcess.Resolve.Spread    (resolveSpread)
import           Data.Morpheus.PreProcess.Selection         (lookupSelectionField, lookupSelectionObjectFieldType)
import           Data.Morpheus.Schema.Internal.Types        (GObject (..), ObjectField (..), OutputObject, TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.Types        as AST (Field (..))
import           Data.Morpheus.Schema.TypeKind              (TypeKind (..))
import           Data.Morpheus.Types.Error                  (Validation)
import           Data.Morpheus.Types.Query.Fragment         (FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection     (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection        (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types                  (Variables)
import           Data.Text                                  (Text)

resolveVariableAndSpread ::
     TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> (Text, RawSelection) -> Validation SelectionSet
resolveVariableAndSpread lib' fragments' variables' parent' (key', RawSelectionSet rawArgs rawSelectors position') = do
  field' <- lookupSelectionField position' key' parent'
  case AST.kind $ fieldContent field' of
    UNION -> pure [(key', SelectionSet [] [] position')] -- TODO: fix it
    OBJECT -> do
      fieldType' <- lookupSelectionObjectFieldType position' key' lib' parent'
      args' <- resolveArguments variables' rawArgs
      sel <- concat <$> mapM (resolveVariableAndSpread lib' fragments' variables' fieldType') rawSelectors
      pure [(key', SelectionSet args' sel position')]
    _ -> Left $ hasNoSubfields key' (AST.fieldType $ fieldContent field') position'
resolveVariableAndSpread _ _ variables' _ (sKey, RawField rawArgs field sPos) = do
  args' <- resolveArguments variables' rawArgs
  pure [(sKey, Field args' field sPos)]
resolveVariableAndSpread lib' fragments' variables' parent' (spreadID, Spread _ sPos) =
  concat <$> (resolveSpread fragments' parent' sPos spreadID >>= recursiveResolve)
  where
    recursiveResolve = mapM (resolveVariableAndSpread lib' fragments' variables' parent')

resolveRawQuery :: TypeLib -> FragmentLib -> Variables -> RawSelectionSet -> OutputObject -> Validation SelectionSet
resolveRawQuery lib' fragments' variables' sel query' =
  concat <$> mapM (resolveVariableAndSpread lib' fragments' variables' query') sel
