{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Selection
  ( validateSelectionSet
  , resolveSelection
  ) where

import           Data.Morpheus.Error.Selection            (duplicateQuerySelections, hasNoSubfields)
import           Data.Morpheus.Schema.Internal.AST        (Core (..), GObject (..), ObjectField (..), OutputObject,
                                                           TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.AST        as AST (Field (..))
import           Data.Morpheus.Schema.TypeKind            (TypeKind (..))
import           Data.Morpheus.Types.Core                 (EnhancedKey (..))
import           Data.Morpheus.Types.Error                (Validation)
import           Data.Morpheus.Types.Query.Fragment       (FragmentLib)
import           Data.Morpheus.Types.Query.RawSelection   (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection      (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types                (Variables)
import           Data.Morpheus.Validation.Arguments       (resolveArguments, validateArguments)
import           Data.Morpheus.Validation.Spread          (resolveSpread)
import           Data.Morpheus.Validation.Utils.Selection (lookupFieldAsSelectionSet, lookupSelectionField,
                                                           lookupSelectionObjectFieldType, notObject)
import           Data.Morpheus.Validation.Utils.Utils     (checkNameCollision)
import           Data.Text                                (Text)

selToKey :: (Text, Selection) -> EnhancedKey
selToKey (sName, Field _ _ pos)        = EnhancedKey sName pos
selToKey (sName, SelectionSet _ _ pos) = EnhancedKey sName pos

checkDuplicatesOn :: GObject ObjectField -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn (GObject _ core) keys = checkNameCollision enhancedKeys (map fst keys) error' >> pure keys
  where
    error' = duplicateQuerySelections (name core)
    enhancedKeys = map selToKey keys

validateBySchema :: TypeLib -> GObject ObjectField -> (Text, Selection) -> Validation (Text, Selection)
validateBySchema lib' parent' (key', SelectionSet args' selectors position') = do
  field' <- lookupSelectionField position' key' parent'
  case AST.kind $ fieldContent field' of
    UNION -> pure (key', SelectionSet args' selectors position')
    OBJECT -> do
      fieldType' <- lookupFieldAsSelectionSet position' key' lib' field'
      arguments' <- validateArguments lib' (key', field') position' args'
      selectorsQS <- validateSelectionSet lib' fieldType' selectors
      pure (key', SelectionSet arguments' selectorsQS position')
    _ -> Left $ hasNoSubfields key' (AST.fieldType $ fieldContent field') position'
validateBySchema lib' parent' (key', Field args' field position') = do
  field' <- lookupSelectionField position' key' parent' >>= notObject (key', position')
  arguments' <- validateArguments lib' (key', field') position' args'
  pure (key', Field arguments' field position')

validateSelectionSet :: TypeLib -> GObject ObjectField -> SelectionSet -> Validation SelectionSet
validateSelectionSet typeLib type' selectors =
  checkDuplicatesOn type' selectors >>= mapM (validateBySchema typeLib type')

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

resolveSelection :: TypeLib -> FragmentLib -> Variables -> RawSelectionSet -> OutputObject -> Validation SelectionSet
resolveSelection lib' fragments' variables' sel query' =
  concat <$> mapM (resolveVariableAndSpread lib' fragments' variables' query') sel
