{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Compile
  ( validateWith,
  )
where

--
--  Morpheus

import Data.Morpheus.Client.Selection
  ( operationTypes,
  )
import Data.Morpheus.Error.Warning
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Parsing.Internal
  ( parseRequest,
  )
import Data.Morpheus.Types.IO (GQLRequest (..))
import qualified Data.Morpheus.Types.Internal.AST as O
  ( Operation (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ClientQuery (..),
    GQLQuery (..),
    Schema,
    VALIDATION_MODE (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import qualified Data.Text as T
  ( pack,
  )
import Language.Haskell.TH

validateWith :: Schema -> (GQLQuery, String) -> Eventless ClientQuery
validateWith schema (rawRequest@GQLQuery {operation}, queryText) = do
  validOperation <- validateRequest schema WITHOUT_VARIABLES rawRequest
  (queryArgsType, queryTypes) <-
    operationTypes
      schema
      (O.operationArguments operation)
      validOperation
  return ClientQuery {queryText, queryTypes, queryArgsType}
