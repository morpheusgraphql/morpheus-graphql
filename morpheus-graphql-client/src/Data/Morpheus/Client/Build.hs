{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Build
  ( defineQuery,
  )
where

import Data.Morpheus.Client.Declare.Client
  ( declareClient,
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientDefinition (..),
    Mode,
  )
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

defineQuery :: Mode -> IO (GQLResult (Schema VALID)) -> (ExecutableDocument, String) -> Q [Dec]
defineQuery mode ioSchema (query, src) = do
  schema <- runIO ioSchema
  case schema >>= (`validateWith` query) of
    Failure errors -> fail (renderGQLErrors errors)
    Success
      { result,
        warnings
      } -> gqlWarnings warnings >> declareClient mode src result

validateWith :: Schema VALID -> ExecutableDocument -> GQLResult ClientDefinition
validateWith
  schema
  rawRequest@ExecutableDocument
    { operation = Operation {operationArguments}
    } = do
    validOperation <- validateRequest Config {debug = False, validationMode = WITHOUT_VARIABLES} schema rawRequest
    toClientDefinition
      schema
      operationArguments
      validOperation
