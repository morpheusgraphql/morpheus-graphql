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
                                                , Schema(..)
                                                , GQLQuery(..)
                                                , VALIDATION_MODE
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Context(..)
                                                , runValidator
                                                , SelectionContext(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Eventless )
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
  -> Eventless (Operation VALID)
validateRequest 
  schema 
  validationMode 
  GQLQuery 
    { fragments
    , inputVariables, 
    operation = operation@Operation 
      { operationName
      , operationSelection
      , operationPosition 
      } 
    }
  = do
      variables <- runValidator validateHelpers ctx ()
      runValidator 
        (validateOperation operation) 
        ctx 
        SelectionContext 
          { variables }
   where 
    ctx = Context 
        { schema 
        , fragments
        , scopeTypeName = "Root"
        , scopeSelectionName = "Root"
        , scopePosition = operationPosition
        , operationName
        }
    validateHelpers = 
        validateFragments operationSelection *>
        resolveOperationVariables
          (fromList inputVariables)
          validationMode
          operation