{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Validation
  ( validateRequest
  ) where

import           Data.Map                                   (fromList)
import           Data.Morpheus.Error.Mutation               (mutationIsNotDefined)
import           Data.Morpheus.Error.Subscription           (subscriptionIsNotDefined)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..), RawOperator, RawOperator',
                                                             ValidOperator)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataOutputObject, DataTypeLib (..))
import           Data.Morpheus.Types.Internal.Validation    (Validation)
import           Data.Morpheus.Types.Types                  (GQLQueryRoot (..))
import           Data.Morpheus.Validation.Fragment          (validateFragments)
import           Data.Morpheus.Validation.Selection         (validateSelectionSet)
import           Data.Morpheus.Validation.Variable          (resolveOperatorVariables)

updateQuery :: RawOperator -> SelectionSet -> ValidOperator
updateQuery (Query (Operator' name' _ _ pos)) sel        = Query (Operator' name' [] sel pos)
updateQuery (Mutation (Operator' name' _ _ pos)) sel     = Mutation (Operator' name' [] sel pos)
updateQuery (Subscription (Operator' name' _ _ pos)) sel = Subscription (Operator' name' [] sel pos)

getOperator :: RawOperator -> DataTypeLib -> Validation (DataOutputObject, RawOperator')
getOperator (Query operator') lib' = pure (snd $ query lib', operator')
getOperator (Mutation operator') lib' =
  case mutation lib' of
    Just (_, mutation') -> pure (mutation', operator')
    Nothing             -> Left $ mutationIsNotDefined (operatorPosition operator')
getOperator (Subscription operator') lib' =
  case subscription lib' of
    Just (_, subscription') -> pure (subscription', operator')
    Nothing                 -> Left $ subscriptionIsNotDefined (operatorPosition operator')

validateRequest :: DataTypeLib -> GQLQueryRoot -> Validation ValidOperator
validateRequest lib' root' = do
  (operatorType', operator') <- getOperator (queryBody root') lib'
  variables' <- resolveOperatorVariables lib' (fragments root') (fromList $ inputVariables root') operator'
  validateFragments lib' root'
  selectors <-
    validateSelectionSet
      lib'
      (fragments root')
      (operatorName operator')
      variables'
      operatorType'
      (operatorSelection operator')
  pure $ updateQuery (queryBody root') selectors
