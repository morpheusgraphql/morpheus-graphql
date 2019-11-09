{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Query.Validation
  ( validateRequest
  )
where

import           Data.Map                       ( fromList )
import           Data.Morpheus.Types.Internal.AST.Operation
                                                ( Operation(..)
                                                , ValidOperation
                                                , getOperationName
                                                , getOperationDataType
                                                )
import           Data.Morpheus.Types.Internal.Data
                                                ( DataTypeLib(..) )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation )
import           Data.Morpheus.Types.Types      ( GQLQueryRoot(..) )
import           Data.Morpheus.Validation.Internal.Utils
                                                ( VALIDATION_MODE )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( validateFragments )
import           Data.Morpheus.Validation.Query.Selection
                                                ( validateSelectionSet )
import           Data.Morpheus.Validation.Query.Variable
                                                ( resolveOperationVariables )


validateRequest
  :: DataTypeLib -> VALIDATION_MODE -> GQLQueryRoot -> Validation ValidOperation
validateRequest lib validationMode GQLQueryRoot { fragments, inputVariables, operation = rawOperation@Operation { operationName, operationType, operationSelection, operationPosition } }
  = do
    operationDataType <- getOperationDataType rawOperation lib
    variables         <- resolveOperationVariables lib
                                                   fragments
                                                   (fromList inputVariables)
                                                   validationMode
                                                   rawOperation
    validateFragments lib fragments operationSelection
    selection <- validateSelectionSet lib
                                      fragments
                                      (getOperationName operationName)
                                      variables
                                      operationDataType
                                      operationSelection
    pure $ Operation { operationName
                     , operationType
                     , operationArgs      = []
                     , operationSelection = selection
                     , operationPosition
                     }
