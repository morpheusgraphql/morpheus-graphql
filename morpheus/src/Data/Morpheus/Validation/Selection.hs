{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Selection
  ( validateSelectionSet
  ) where

import           Data.Morpheus.Error.Selection            (duplicateQuerySelections, hasNoSubfields)
import           Data.Morpheus.Schema.Internal.AST        (Core (..), GObject (..), ObjectField (..), OutputObject,
                                                           TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.AST        as AST (Field (..))
import           Data.Morpheus.Schema.TypeKind            (TypeKind (..))
import           Data.Morpheus.Types.Core                 (EnhancedKey (..))
import           Data.Morpheus.Types.Error                (Validation)
import           Data.Morpheus.Types.Query.Fragment       (Fragment, FragmentLib, RawFragment)
import qualified Data.Morpheus.Types.Query.Fragment       as F (Fragment (..))
import           Data.Morpheus.Types.Query.RawSelection   (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection      (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types                (Variables)
import           Data.Morpheus.Validation.Arguments       (resolveArguments, validateArguments)
import           Data.Morpheus.Validation.Spread          (resolveSpread)
import           Data.Morpheus.Validation.Utils.Selection (lookupFieldAsSelectionSet, lookupSelectionField, notObject)
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

validateSelectionSet ::
     TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> RawSelectionSet -> Validation SelectionSet
validateSelectionSet lib' fragments' variables' type'
  -- checkDuplicatesOn type' selectors >>= todo : make it work again
 = mapM (validateSelection lib' fragments' variables' type')

validateSelection ::
     TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> (Text, RawSelection) -> Validation (Text, Selection)
validateSelection lib' fragments' variables' parent' (key', RawSelectionSet rawArgs rawSelectors position') = do
  field' <- lookupSelectionField position' key' parent'
  case AST.kind $ fieldContent field' of
    UNION -> pure (key', SelectionSet [] [] position') -- TODO: implement it
    OBJECT -> do
      fieldType' <- lookupFieldAsSelectionSet position' key' lib' field'
      resolvedArgs' <- resolveArguments variables' rawArgs
      arguments' <- validateArguments lib' (key', field') position' resolvedArgs'
      selections' <- validateSelectionSet lib' fragments' variables' fieldType' rawSelectors
      pure (key', SelectionSet arguments' selections' position')
    _ -> Left $ hasNoSubfields key' (AST.fieldType $ fieldContent field') position'
validateSelection lib' _ variables' parent' (key', RawField rawArgs field position') = do
  field' <- lookupSelectionField position' key' parent' >>= notObject (key', position')
  args' <- resolveArguments variables' rawArgs
  arguments' <- validateArguments lib' (key', field') position' args'
  pure (key', Field arguments' field position')
validateSelection lib' fragments' variables' parent' (key', Spread id' position') = do
  rawFragment' <- resolveSpread fragments' parent' position' id'
  fragment' <- castFragment lib' fragments' variables' parent' rawFragment'
  pure $ (key', FragmentSpread fragment')

castFragment ::
     TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> RawFragment -> Validation (Fragment SelectionSet)
castFragment lib' fragments' variables' onType' fragment' = do
  selection' <- validateSelectionSet lib' fragments' variables' onType' (F.content fragment')
  return (fragment' {F.content = selection'})
