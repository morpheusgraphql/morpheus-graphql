{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Query.Validation
  ( validateRequest
  )
where

import           Data.Map                       ( fromList )
import           Data.Morpheus.Types.Internal.AST
                                                ( Operation(..)
                                                , ValidOperation
                                                , getOperationName
                                                , getOperationObject
                                                , DataTypeLib(..)
                                                , GQLQuery(..)
                                                , VALIDATION_MODE
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( validateFragments )
import           Data.Morpheus.Validation.Query.Selection
                                                ( validateSelectionSet )
import           Data.Morpheus.Validation.Query.Variable
                                                ( resolveOperationVariables )


validateRequest
  :: DataTypeLib -> VALIDATION_MODE -> GQLQuery -> Validation ValidOperation
validateRequest lib validationMode GQLQuery { fragments, inputVariables, operation = rawOperation@Operation { operationName, operationType, operationSelection, operationPosition } }
  = do
    operationDataType <-  getOperationObject rawOperation lib
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
                     , operationArguments      = []
                     , operationSelection = selection
                     , operationPosition
                     }
