{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Validation
  ( validateRequest
  ) where

import           Data.Map                               (fromList)
import           Data.Morpheus.Error.Mutation           (mutationIsNotDefined)
import           Data.Morpheus.Error.Subscription       (subscriptionIsNotDefined)
import           Data.Morpheus.Schema.Internal.AST      (GObject (..), ObjectField (..), OutputObject, TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.AST      as SC (Field (..))
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.Query.Operator     (Operator (..), RawOperator, ValidOperator, VariableDefinitions)
import           Data.Morpheus.Types.Query.RawSelection (RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (SelectionSet)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Morpheus.Validation.Fragment      (validateFragments)
import           Data.Morpheus.Validation.Selection     (validateSelectionSet)
import           Data.Morpheus.Validation.Variable      (allVariableReferences, resolveOperationVariables)
import           Data.Text                              (Text)

updateQuery :: RawOperator -> SelectionSet -> ValidOperator
updateQuery (Query name' _ _ pos) sel        = Query name' [] sel pos
updateQuery (Mutation name' _ _ pos) sel     = Mutation name' [] sel pos
updateQuery (Subscription name' _ _ pos) sel = Subscription name' [] sel pos

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
getOperator (Subscription _ args' sel position') lib' =
  case subscription lib' of
    Just (_, subscription') -> pure (subscription', args', sel)
    Nothing                 -> Left $ subscriptionIsNotDefined position'

validateRequest :: TypeLib -> GQLQueryRoot -> Validation ValidOperator
validateRequest lib' root' = do
  (operator', args', rawSelection') <- getOperator (queryBody root') lib'
  variables' <-
    resolveOperationVariables lib' (fromList $ inputVariables root') (allVariableReferences [rawSelection']) args'
  validateFragments lib' root'
  selectors <- validateSelectionSet lib' (fragments root') variables' (setFieldSchema operator') rawSelection'
  pure $ updateQuery (queryBody root') selectors
