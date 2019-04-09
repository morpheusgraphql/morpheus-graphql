{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.PreProcess.PreProcess
  ( preProcessQuery
  ) where

import           Data.Morpheus.Error.Mutation           (mutationIsNotDefined)
import           Data.Morpheus.Error.Selection          (duplicateQuerySelections)
import           Data.Morpheus.PreProcess.Arguments     (validateArguments)
import           Data.Morpheus.PreProcess.Fragment      (validateFragments)
import           Data.Morpheus.PreProcess.Selection     (lookupFieldAsSelectionSet, lookupSelectionField, mustBeObject,
                                                         notObject)
import           Data.Morpheus.PreProcess.Spread        (prepareRawSelection)
import           Data.Morpheus.PreProcess.Utils         (checkNameCollision)
import           Data.Morpheus.PreProcess.Variable      (validateDefinedVariables)
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), ObjectField (..), OutputObject,
                                                         TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.Types    as SC (Field (..))
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.Query.Operator     (Operator (..), RawOperator, ValidOperator, VariableDefinitions)
import           Data.Morpheus.Types.Query.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

mapSelectors :: TypeLib -> GObject ObjectField -> SelectionSet -> Validation SelectionSet
mapSelectors typeLib type' selectors = checkDuplicatesOn type' selectors >>= mapM (validateBySchema typeLib type')

validateBySchema :: TypeLib -> GObject ObjectField -> (Text, Selection) -> Validation (Text, Selection)
validateBySchema lib' parent' (key', SelectionSet args' selectors position') = do
  field' <- lookupSelectionField position' key' parent' >>= mustBeObject (key', position')
  fieldType' <- lookupFieldAsSelectionSet position' key' lib' field'
  arguments' <- validateArguments lib' (key', field') position' args'
  selectorsQS <- mapSelectors lib' fieldType' selectors
  pure (key', SelectionSet arguments' selectorsQS position')
validateBySchema lib' parent' (key', Field args' field position') = do
  field' <- lookupSelectionField position' key' parent' >>= notObject (key', position')
  arguments' <- validateArguments lib' (key', field') position' args'
  pure (key', Field arguments' field position')

selToKey :: (Text, Selection) -> EnhancedKey
selToKey (sName, Field _ _ pos)        = EnhancedKey sName pos
selToKey (sName, SelectionSet _ _ pos) = EnhancedKey sName pos

checkDuplicatesOn :: GObject ObjectField -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn (GObject _ core) keys = checkNameCollision enhancedKeys (map fst keys) error' >> pure keys
  where
    error' = duplicateQuerySelections (name core)
    enhancedKeys = map selToKey keys

updateQuery :: RawOperator -> SelectionSet -> ValidOperator
updateQuery (Query name' _ _ pos) sel    = Query name' [] sel pos
updateQuery (Mutation name' _ _ pos) sel = Mutation name' [] sel pos

fieldSchema :: [(Text, ObjectField)]
fieldSchema =
  [ ( "__schema"
    , ObjectField
        { args = []
        , fieldContent =
            SC.Field
              { SC.fieldName = "__schema"
              , SC.notNull = True
              , SC.asList = False
              , SC.kind = OBJECT
              , SC.fieldType = "__Schema"
              }
        })
  ]

setFieldSchema :: GObject ObjectField -> GObject ObjectField
setFieldSchema (GObject fields core) = GObject (fields ++ fieldSchema) core

getOperator :: RawOperator -> TypeLib -> Validation (OutputObject, VariableDefinitions, RawSelectionSet)
getOperator (Query _ args' sel _) lib' = pure (snd $ query lib', args', sel)
getOperator (Mutation _ args' sel position') lib' =
  case mutation lib' of
    Just (_, mutation') -> pure (mutation', args', sel)
    Nothing             -> Left $ mutationIsNotDefined position'

preProcessQuery :: TypeLib -> GQLQueryRoot -> Validation ValidOperator
preProcessQuery lib root = do
  (query', args', rawSel) <- getOperator (queryBody root) lib
  validateDefinedVariables lib root args'
  validateFragments lib root
  sel <- prepareRawSelection lib root rawSel (setFieldSchema query')
  selectors <- mapSelectors lib (setFieldSchema query') sel
  pure $ updateQuery (queryBody root) selectors
