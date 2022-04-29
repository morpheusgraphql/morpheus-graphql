{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Build
  ( defineQueryTypes,
    defineGlobalTypes,
  )
where

import Data.Morpheus.Client.Declare.Client
  ( declareClient,
    declareTypes,
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientDefinition (..),
    Mode,
  )
import Data.Morpheus.Client.Transform.Inputs (toGlobalDefinitions)
import Data.Morpheus.Client.Transform.Selection
  ( toClientDefinition,
  )
import Data.Morpheus.Core
  ( Config (..),
    VALIDATION_MODE (..),
    validateRequest,
  )
import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ExecutableDocument (..),
    Operation (..),
    Schema,
    VALID,
  )
import Language.Haskell.TH
import Relude

handleResult :: GQLResult t -> (t -> Q a) -> Q a
handleResult x f = case x of
  Failure errors -> fail (renderGQLErrors errors)
  Success
    { result,
      warnings
    } -> gqlWarnings warnings >> f result

defineGlobalTypes :: GQLResult (Schema VALID) -> Q [Dec]
defineGlobalTypes schema =
  handleResult
    (schema >>= toGlobalDefinitions)
    declareTypes

defineQueryTypes :: Mode -> GQLResult (Schema VALID) -> (ExecutableDocument, String) -> Q [Dec]
defineQueryTypes mode schema (query, src) =
  handleResult
    (schema >>= validateWith mode query)
    (declareClient src)

validateWith :: Mode -> ExecutableDocument -> Schema VALID -> GQLResult ClientDefinition
validateWith
  mode
  rawRequest@ExecutableDocument {operation}
  schema =
    do
      validOperation <-
        validateRequest
          Config {debug = False, validationMode = WITHOUT_VARIABLES}
          schema
          rawRequest
      toClientDefinition
        mode
        schema
        (operationArguments operation)
        validOperation
