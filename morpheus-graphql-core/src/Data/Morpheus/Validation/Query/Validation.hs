{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
where

import Data.HashMap.Lazy (fromList)
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery (..),
    Operation (..),
    Schema (..),
    TypeKind (..),
    VALID,
    VALIDATION_MODE,
  )
import Data.Morpheus.Types.Internal.Config (Config (..))
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Types.Internal.Validation
  ( CurrentSelection (..),
    OperationContext (..),
    Scope (..),
    ScopeKind (..),
    runValidator,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( validateFragments,
  )
import Data.Morpheus.Validation.Query.Selection
  ( validateOperation,
  )
import Data.Morpheus.Validation.Query.Variable
  ( resolveOperationVariables,
  )

validateRequest ::
  Config ->
  Schema VALID ->
  VALIDATION_MODE ->
  GQLQuery ->
  Eventless (Operation VALID)
validateRequest
  config
  schema
  validationMode
  GQLQuery
    { fragments,
      inputVariables,
      operation =
        operation@Operation
          { operationName,
            operationSelection,
            operationPosition
          }
    } =
    do
      variables <- runValidator validateHelpers config schema scope (ctx ())
      runValidator (validateOperation operation) config schema scope (ctx variables)
    where
      scope =
        Scope
          { kind = SELECTION,
            currentTypeName = "Root",
            currentTypeKind = KindObject Nothing,
            currentTypeWrappers = [],
            fieldname = "Root",
            position = Just operationPosition
          }
      ctx variables =
        OperationContext
          { fragments,
            selection = CurrentSelection {operationName},
            variables
          }
      validateHelpers =
        validateFragments operationSelection
          *> resolveOperationVariables
            (fromList inputVariables)
            validationMode
            operation
