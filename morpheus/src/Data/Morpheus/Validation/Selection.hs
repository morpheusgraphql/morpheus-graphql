{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Selection
  ( validateSelectionSet
  ) where

import           Data.Morpheus.Error.Internal             (internalError)
import           Data.Morpheus.Error.Selection            (duplicateQuerySelections, hasNoSubfields)
import           Data.Morpheus.Schema.Internal.AST        (Core (..), GObject (..), ObjectField (..), OutputObject,
                                                           TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.AST        as AST (Field (..), OutputObject (..))
import           Data.Morpheus.Schema.TypeKind            (TypeKind (..))
import           Data.Morpheus.Types.Core                 (EnhancedKey (..))
import           Data.Morpheus.Types.Error                (Validation)
import           Data.Morpheus.Types.Query.Fragment       (FragmentLib, RawFragment)
import qualified Data.Morpheus.Types.Query.Fragment       as F (Fragment (..))
import           Data.Morpheus.Types.Query.RawSelection   (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection      (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types                (Variables)
import           Data.Morpheus.Validation.Arguments       (resolveArguments, validateArguments)
import           Data.Morpheus.Validation.Spread          (resolveSpread)
import           Data.Morpheus.Validation.Utils.Selection (lookupFieldAsSelectionSet, lookupPossibleTypeKeys,
                                                           lookupPossibleTypes, lookupSelectionField, notObject)
import           Data.Morpheus.Validation.Utils.Utils     (checkNameCollision)
import           Data.Text                                (Text)
import           Debug.Trace

selToKey :: (Text, Selection) -> EnhancedKey
selToKey (sName, Field _ _ pos)          = EnhancedKey sName pos
selToKey (sName, SelectionSet _ _ pos)   = EnhancedKey sName pos
selToKey (sName, UnionSelection _ _ pos) = EnhancedKey sName pos

checkDuplicatesOn :: GObject ObjectField -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn (GObject _ core) keys = checkNameCollision enhancedKeys (map fst keys) error' >> pure keys
  where
    error' = duplicateQuerySelections (name core)
    enhancedKeys = map selToKey keys

validateSelectionSet ::
     TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> RawSelectionSet -> Validation SelectionSet
validateSelectionSet lib' fragments' variables' type' selection' =
  concat <$> mapM (validateSelection lib' fragments' variables' type') selection' >>= checkDuplicatesOn type'

castFragment :: TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> RawFragment -> Validation SelectionSet
castFragment lib' fragments' variables' onType' fragment' =
  validateSelectionSet lib' fragments' variables' onType' (F.content fragment')

isolateFragment :: FragmentLib -> [Text] -> (Text, RawSelection) -> Validation [RawFragment]
isolateFragment fragments' posTypes' (_, Spread key' position') = do
  fragment' <- resolveSpread fragments' posTypes' position' key'
  return [fragment']
isolateFragment _ _ _ = internalError "selection without fragment are not allowed on union type"

categorizeType :: [RawFragment] -> OutputObject -> (OutputObject, [RawFragment])
categorizeType fragments' type'@(GObject _ core) = (type', filter matches fragments')
  where
    matches fragment' = F.key fragment' == name core

categorizeTypes :: [OutputObject] -> [RawFragment] -> [(OutputObject, [RawFragment])]
categorizeTypes types' fragments' = map (categorizeType fragments') types'

validateSelection ::
     TypeLib -> FragmentLib -> Variables -> GObject ObjectField -> (Text, RawSelection) -> Validation SelectionSet
validateSelection lib' fragments' variables' parent' (key', RawSelectionSet rawArgs rawSelectors position') = do
  field' <- lookupSelectionField position' key' parent'
  resolvedArgs' <- resolveArguments variables' rawArgs
  arguments' <- validateArguments lib' (key', field') position' resolvedArgs'
  case AST.kind $ fieldContent field' of
    UNION -> do
      keys' <- lookupPossibleTypeKeys position' key' lib' field'
      spreads' <- concat <$> mapM (isolateFragment fragments' keys') rawSelectors
      possibleFieldTypes' <- lookupPossibleTypes position' key' lib' keys'
      let zippedSpreads' = categorizeTypes possibleFieldTypes' spreads'
      --mapM  validateSelectionSet  lib' fragments' variables' fieldType' rawSelectors
      pure (trace (show zippedSpreads') [(key', UnionSelection [] [] position')]) -- TODO: implement it
    OBJECT -> do
      fieldType' <- lookupFieldAsSelectionSet position' key' lib' field'
      selections' <- validateSelectionSet lib' fragments' variables' fieldType' rawSelectors
      pure [(key', SelectionSet arguments' selections' position')]
    _ -> Left $ hasNoSubfields key' (AST.fieldType $ fieldContent field') position'
validateSelection lib' _ variables' parent' (key', RawField rawArgs field position') = do
  field' <- lookupSelectionField position' key' parent' >>= notObject (key', position')
  args' <- resolveArguments variables' rawArgs
  arguments' <- validateArguments lib' (key', field') position' args'
  pure [(key', Field arguments' field position')]
validateSelection lib' fragments' variables' parent'@(GObject _ core) (key', Spread _ position') =
  resolveSpread fragments' [name core] position' key' >>= castFragment lib' fragments' variables' parent'
