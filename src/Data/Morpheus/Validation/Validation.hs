{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Validation
  ( validateRequest
  ) where

import           Data.Map                                   (fromList)
import           Data.Morpheus.Error.Mutation               (mutationIsNotDefined)
import           Data.Morpheus.Error.Subscription           (subscriptionIsNotDefined)
import           Data.Morpheus.Types.Internal.AST.Operation (Operation (..), OperationKind (..), RawOperation,
                                                             ValidOperation)
import           Data.Morpheus.Types.Internal.Data          (DataOutputObject, DataTypeLib (..))
import           Data.Morpheus.Types.Internal.Validation    (Validation)
import           Data.Morpheus.Types.Types                  (GQLQueryRoot (..))
import           Data.Morpheus.Validation.Fragment          (validateFragments)
import           Data.Morpheus.Validation.Selection         (validateSelectionSet)
import           Data.Morpheus.Validation.Variable          (resolveOperationVariables)

getOperationDataType :: RawOperation -> DataTypeLib -> Validation DataOutputObject
getOperationDataType Operation {operationKind = QUERY} lib = pure $ snd $ query lib
getOperationDataType Operation {operationKind = MUTATION, operationPosition} lib =
  case mutation lib of
    Just (_, mutation') -> pure mutation'
    Nothing             -> Left $ mutationIsNotDefined operationPosition
getOperationDataType Operation {operationKind = SUBSCRIPTION, operationPosition} lib =
  case subscription lib of
    Just (_, subscription') -> pure subscription'
    Nothing                 -> Left $ subscriptionIsNotDefined operationPosition

validateRequest :: DataTypeLib -> GQLQueryRoot -> Validation ValidOperation
validateRequest lib GQLQueryRoot { fragments
                                 , inputVariables
                                 , operation = rawOperation@Operation { operationName
                                                                      , operationKind
                                                                      , operationSelection
                                                                      , operationPosition
                                                                      }
                                 } = do
  operationDataType <- getOperationDataType rawOperation lib
  variables <- resolveOperationVariables lib fragments (fromList inputVariables) rawOperation
  validateFragments lib fragments operationSelection
  selection <- validateSelectionSet lib fragments operationName variables operationDataType operationSelection
  pure $ Operation {operationName, operationKind, operationArgs = [], operationSelection = selection, operationPosition}
