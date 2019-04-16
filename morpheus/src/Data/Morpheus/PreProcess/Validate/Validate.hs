{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.PreProcess.Validate.Validate
  ( mapSelectorValidation
  ) where

import           Data.Morpheus.Error.Selection               (duplicateQuerySelections)
import           Data.Morpheus.PreProcess.Selection          (lookupFieldAsSelectionSet, lookupSelectionField,
                                                              mustBeObject, notObject)
import           Data.Morpheus.PreProcess.Utils              (checkNameCollision)
import           Data.Morpheus.PreProcess.Validate.Arguments (validateArguments)
import           Data.Morpheus.Schema.Internal.Types         (Core (..), GObject (..), ObjectField (..), TypeLib (..))
import           Data.Morpheus.Types.Core                    (EnhancedKey (..))
import           Data.Morpheus.Types.Error                   (Validation)
import           Data.Morpheus.Types.Query.Selection         (Selection (..), SelectionSet)
import           Data.Text                                   (Text)

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
  field' <- lookupSelectionField position' key' parent' >>= mustBeObject (key', position')
  fieldType' <- lookupFieldAsSelectionSet position' key' lib' field'
  arguments' <- validateArguments lib' (key', field') position' args'
  selectorsQS <- mapSelectorValidation lib' fieldType' selectors
  pure (key', SelectionSet arguments' selectorsQS position')
validateBySchema lib' parent' (key', Field args' field position') = do
  field' <- lookupSelectionField position' key' parent' >>= notObject (key', position')
  arguments' <- validateArguments lib' (key', field') position' args'
  pure (key', Field arguments' field position')

mapSelectorValidation :: TypeLib -> GObject ObjectField -> SelectionSet -> Validation SelectionSet
mapSelectorValidation typeLib type' selectors =
  checkDuplicatesOn type' selectors >>= mapM (validateBySchema typeLib type')
