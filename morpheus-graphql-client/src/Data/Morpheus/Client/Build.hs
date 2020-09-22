{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Client.Build
  ( defineQuery,
  )
where

--
-- MORPHEUS
import Data.Morpheus.Client.Declare.Client
  ( declareClient,
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientDefinition (..),
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
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery (..),
    Operation (..),
    Schema,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Language.Haskell.TH

defineQuery :: IO (Eventless (Schema VALID)) -> (GQLQuery, String) -> Q [Dec]
defineQuery ioSchema (query, src) = do
  schema <- runIO ioSchema
  case schema >>= (`validateWith` query) of
    Failure errors -> fail (renderGQLErrors errors)
    Success
      { result,
        warnings
      } -> gqlWarnings warnings >> declareClient src result

validateWith :: Schema VALID -> GQLQuery -> Eventless ClientDefinition
validateWith
  schema
  rawRequest@GQLQuery
    { operation = Operation {operationArguments}
    } = do
    validOperation <- validateRequest Config {debug = False, validationMode = WITHOUT_VARIABLES} schema rawRequest
    toClientDefinition
      schema
      operationArguments
      validOperation
