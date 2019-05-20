{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Validation
  ( validateRequest
  ) where

import           Data.Map                            (fromList)
import           Data.Morpheus.Error.Mutation        (mutationIsNotDefined)
import           Data.Morpheus.Error.Subscription    (subscriptionIsNotDefined)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.Internal.AST    (ASTField (..), ASTOutputObject, ASTType (..), ASTTypeLib (..))
import           Data.Morpheus.Types.Query.Operator  (Operator (..), Operator' (..), RawOperator, RawOperator',
                                                      TypeWrapper (..), ValidOperator)
import           Data.Morpheus.Types.Query.Selection (SelectionSet)
import           Data.Morpheus.Types.Types           (GQLQueryRoot (..))
import           Data.Morpheus.Validation.Fragment   (validateFragments)
import           Data.Morpheus.Validation.Selection  (validateSelectionSet)
import           Data.Morpheus.Validation.Variable   (resolveOperatorVariables)

updateQuery :: RawOperator -> SelectionSet -> ValidOperator
updateQuery (Query (Operator' name' _ _ pos)) sel        = Query (Operator' name' [] sel pos)
updateQuery (Mutation (Operator' name' _ _ pos)) sel     = Mutation (Operator' name' [] sel pos)
updateQuery (Subscription (Operator' name' _ _ pos)) sel = Subscription (Operator' name' [] sel pos)

setFieldSchema :: ASTOutputObject -> ASTOutputObject
setFieldSchema type' = type' {typeData = ("__schema", __schema) : typeData type'}
  where
    __schema =
      ASTField
        { fieldArgs = []
        , fieldName = "__schema"
        , fieldTypeWrappers = [NonNullType]
        , fieldKind = OBJECT
        , fieldType = "__Schema"
        }

getOperator :: RawOperator -> ASTTypeLib -> Validation (ASTOutputObject, RawOperator')
getOperator (Query operator') lib' = pure (snd $ query lib', operator')
getOperator (Mutation operator') lib' =
  case mutation lib' of
    Just (_, mutation') -> pure (mutation', operator')
    Nothing             -> Left $ mutationIsNotDefined (operatorPosition operator')
getOperator (Subscription operator') lib' =
  case subscription lib' of
    Just (_, subscription') -> pure (subscription', operator')
    Nothing                 -> Left $ subscriptionIsNotDefined (operatorPosition operator')

validateRequest :: ASTTypeLib -> GQLQueryRoot -> Validation ValidOperator
validateRequest lib' root' = do
  (operatorType', operator') <- getOperator (queryBody root') lib'
  variables' <- resolveOperatorVariables lib' (fromList $ inputVariables root') operator'
  validateFragments lib' root'
  selectors <-
    validateSelectionSet lib' (fragments root') variables' (setFieldSchema operatorType') (operatorSelection operator')
  pure $ updateQuery (queryBody root') selectors
