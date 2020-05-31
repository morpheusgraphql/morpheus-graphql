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
  ( validateRequest,
  )
import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery (..),
    Operation (..),
    Schema,
    VALIDATION_MODE (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH

defineQuery :: IO (Eventless Schema) -> (GQLQuery, String) -> Q [Dec]
defineQuery ioSchema (query, src) = do
  schema <- runIO ioSchema
  case schema >>= (`validateWith` query) of
    Failure errors -> fail (renderGQLErrors errors)
    Success
      { result,
        warnings
      } -> gqlWarnings warnings >> declareClient src result

validateWith :: Schema -> GQLQuery -> Eventless ClientDefinition
validateWith
  schema
  rawRequest@GQLQuery
    { operation = Operation {operationArguments}
    } = do
    validOperation <- validateRequest schema WITHOUT_VARIABLES rawRequest
    toClientDefinition
      schema
      operationArguments
      validOperation
