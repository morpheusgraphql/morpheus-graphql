{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
where

import Data.HashMap.Lazy (fromList)
import Data.Morpheus.Ext.Result
  ( Eventless,
  )
import Data.Morpheus.Internal.Utils (empty)
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery (..),
    Operation (..),
    Schema (..),
    TypeKind (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Config (Config (..))
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
import Data.Morpheus.Validation.Query.FragmentPreconditions
  ( checkFragmentPreconditions,
  )
import Data.Morpheus.Validation.Query.Selection
  ( vaidateFragmentSelection,
    validateOperation,
  )
import Data.Morpheus.Validation.Query.Variable
  ( resolveOperationVariables,
  )
import Relude hiding
  ( empty,
    fromList,
  )

validateRequest ::
  Config ->
  Schema VALID ->
  GQLQuery ->
  Eventless (Operation VALID)
validateRequest
  config
  schema
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
      variables <-
        runValidator
          validateHelpers
          config
          schema
          scope
          ( OperationContext
              { selection,
                fragments,
                variables = empty
              }
          )
      validFragments <-
        runValidator
          (validateFragments vaidateFragmentSelection)
          config
          schema
          scope
          ( OperationContext
              { selection,
                fragments,
                variables
              }
          )
      runValidator
        (validateOperation operation)
        config
        schema
        scope
        ( OperationContext
            { selection,
              fragments = validFragments,
              variables
            }
        )
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
      selection = CurrentSelection {operationName}
      validateHelpers =
        checkFragmentPreconditions operationSelection
          *> resolveOperationVariables
            config
            (fromList inputVariables)
            operation
