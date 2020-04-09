{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Data.Morpheus.Validation.Query.Validation
  ( validateRequest
  )
where

import           Data.Map                       ( fromList )
import           Data.Morpheus.Types.Internal.AST
                                                ( Operation(..)
                                                , VALID
                                                , getOperationObject
                                                , Schema(..)
                                                , GQLQuery(..)
                                                , VALIDATION_MODE
                                                , InputSource(..)
                                                , Variables
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( empty )
import           Data.Morpheus.Types.Internal.Validator
                                                ( SelectionValidator
                                                , ValidationContext(..)
                                                , runValidator
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Stateless )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( validateFragments )
import           Data.Morpheus.Validation.Query.Selection
                                                ( validateOperation )
import           Data.Morpheus.Validation.Query.Variable
                                                ( resolveOperationVariables )


validateRequest
  :: Schema 
  -> VALIDATION_MODE 
  -> GQLQuery 
  -> Stateless (Operation VALID)
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
  = runValidator 
      validation
      ValidationContext 
        { schema 
        , fragments
        , operationName
        , scopeTypeName = "Root"
        , scopePosition = operationPosition
        , input = InputSource {
            sourceType = Nothing,
            sourcePath = []
          }
        }
   where
    validateHelpers = 
        validateFragments operationSelection *>
        resolveOperationVariables
          (fromList inputVariables)
          validationMode
          rawOperation
    validation :: SelectionValidator (Operation VALID)
    validation = do
      variables <- validateHelpers 
      operationTypeDef  <-  getOperationObject rawOperation schema
      selection <- validateOperation
                                  variables
                                  operationTypeDef
                                  rawOperation
      pure $ Operation 
              { operationName
              , operationType
              , operationArguments = empty
              , operationSelection = selection
              , operationPosition
              }
