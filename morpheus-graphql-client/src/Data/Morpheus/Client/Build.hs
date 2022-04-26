{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Build
  ( defineQuery,
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

defineGlobalTypes :: IO (GQLResult (Schema VALID)) -> Q [Dec]
defineGlobalTypes ioSchema = do
  schema <- runIO ioSchema
  case schema >>= toGlobalDefinitions of
    Failure errors -> fail (renderGQLErrors errors)
    Success
      { result,
        warnings
      } -> gqlWarnings warnings >> declareTypes result

defineQuery :: Mode -> IO (GQLResult (Schema VALID)) -> (ExecutableDocument, String) -> Q [Dec]
defineQuery mode ioSchema (query, src) = do
  schema <- runIO ioSchema
  case schema >>= (\s -> validateWith mode s query) of
    Failure errors -> fail (renderGQLErrors errors)
    Success
      { result,
        warnings
      } -> gqlWarnings warnings >> declareClient src result

validateWith :: Mode -> Schema VALID -> ExecutableDocument -> GQLResult ClientDefinition
validateWith
  mode
  schema
  rawRequest@ExecutableDocument
    { operation = Operation {operationArguments}
    } = do
    validOperation <- validateRequest Config {debug = False, validationMode = WITHOUT_VARIABLES} schema rawRequest
    toClientDefinition
      mode
      schema
      operationArguments
      validOperation
