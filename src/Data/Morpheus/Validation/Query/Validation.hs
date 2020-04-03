{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data.Morpheus.Validation.Query.Validation
  ( validateRequest
  )
where

import           Data.Map                       ( fromList )
import           Data.Morpheus.Types.Internal.AST
                                                ( Operation(..)
                                                , VALID
                                                , getOperationName
                                                , getOperationObject
                                                , Schema(..)
                                                , GQLQuery(..)
                                                , VALIDATION_MODE
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( empty )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , ValidationContext(..)
                                                , runValidation
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Stateless )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( validateFragments )
import           Data.Morpheus.Validation.Query.Selection
                                                ( validateSelectionSet )
import           Data.Morpheus.Validation.Query.Variable
                                                ( resolveOperationVariables )


validateRequest
  :: Schema -> VALIDATION_MODE -> GQLQuery -> Stateless (Operation VALID)
validateRequest 
  schema 
  validationMode 
  GQLQuery 
    { fragments
    , inputVariables, 
    operation = rawOperation@Operation 
      { operationName
      , operationType
      , operationSelection
      , operationPosition 
      } 
    }
  = runValidation 
      validation
      ValidationContext 
        { schema 
        , operationName
        , scopePosition = operationPosition
        }
   where
    validation :: Validation (Operation VALID)
    validation = do
      operationDataType <-  getOperationObject rawOperation schema
      variables         <- resolveOperationVariables
                                  fragments
                                  (fromList inputVariables)
                                  validationMode
                                  rawOperation
      validateFragments schema fragments operationSelection
      selection <- validateSelectionSet 
                                  schema
                                  fragments
                                  (getOperationName operationName)
                                  variables
                                  operationDataType
                                  operationSelection
      pure $ Operation { operationName
                      , operationType
                      , operationArguments      = empty
                      , operationSelection = selection
                      , operationPosition
                      }
