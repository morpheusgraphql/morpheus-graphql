{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Query.Validation
  ( validateRequest
  ) where

import           Data.Map                                   (fromList)
import           Data.Morpheus.Error.Mutation               (mutationIsNotDefined)
import           Data.Morpheus.Error.Subscription           (subscriptionIsNotDefined)
import           Data.Morpheus.Types.Internal.AST.Operation (Operation (..), RawOperation, ValidOperation)
import           Data.Morpheus.Types.Internal.Data          (DataObject, DataTypeLib (..), OperationKind (..))
import           Data.Morpheus.Types.Internal.Validation    (Validation)
import           Data.Morpheus.Types.Types                  (GQLQueryRoot (..))
import           Data.Morpheus.Validation.Internal.Utils    (VALIDATION_MODE)
import           Data.Morpheus.Validation.Query.Fragment    (validateFragments)
import           Data.Morpheus.Validation.Query.Selection   (validateSelectionSet)
import           Data.Morpheus.Validation.Query.Variable    (resolveOperationVariables)

getOperationDataType :: RawOperation -> DataTypeLib -> Validation DataObject
getOperationDataType Operation {operationKind = Query} lib = pure $ snd $ query lib
getOperationDataType Operation {operationKind = Mutation, operationPosition} lib =
  case mutation lib of
    Just (_, mutation') -> pure mutation'
    Nothing             -> Left $ mutationIsNotDefined operationPosition
getOperationDataType Operation {operationKind = Subscription, operationPosition} lib =
  case subscription lib of
    Just (_, subscription') -> pure subscription'
    Nothing                 -> Left $ subscriptionIsNotDefined operationPosition

validateRequest :: DataTypeLib -> VALIDATION_MODE -> GQLQueryRoot -> Validation ValidOperation
validateRequest lib validationMode GQLQueryRoot { fragments
                                                , inputVariables
                                                , operation = rawOperation@Operation { operationName
                                                                                     , operationKind
                                                                                     , operationSelection
                                                                                     , operationPosition
                                                                                     }
                                                } = do
  operationDataType <- getOperationDataType rawOperation lib
  variables <- resolveOperationVariables lib fragments (fromList inputVariables) validationMode rawOperation
  validateFragments lib fragments operationSelection
  selection <- validateSelectionSet lib fragments operationName variables operationDataType operationSelection
  pure $ Operation {operationName, operationKind, operationArgs = [], operationSelection = selection, operationPosition}
