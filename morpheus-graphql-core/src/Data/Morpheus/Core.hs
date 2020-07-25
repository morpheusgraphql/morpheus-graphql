{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Core
  ( runApi,
    parseDSL,
    parseFullGQLDocument,
    parseGQLDocument,
    parseTypeSystemDefinition,
    parseTypeDefinitions,
    validateRequest,
    parseRequestWith,
    validateSchema,
    parseRequest,
    RenderGQL (..),
    SelectionTree (..),
  )
where

-- MORPHEUS
import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Internal.Utils
  ( (<:>),
    empty,
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseRequestWith,
    parseTypeDefinitions,
    parseTypeSystemDefinition,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Schema.Schema (internalSchema)
import Data.Morpheus.Schema.SchemaAPI (withSystemFields)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Operation (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    VALID,
    Value,
  )
import Data.Morpheus.Types.Internal.Config (Config (..))
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    ResolverContext (..),
    ResponseStream,
    ResultT (..),
    RootResModel,
    cleanEvents,
    resultOr,
    runRootResModel,
  )
import Data.Morpheus.Types.SelectionTree (SelectionTree (..))
import Data.Morpheus.Validation.Document.Validation (ValidateSchema (..))
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import qualified Data.Text.Lazy as LT
  ( toStrict,
  )
import Data.Text.Lazy.Encoding (decodeUtf8)

runApi ::
  forall event m s.
  (Monad m, ValidateSchema s) =>
  Schema s ->
  RootResModel event m ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
runApi inputSchema resModel request = do
  let config = Config {debug = False}
  validRequest <- validateReq config inputSchema request
  resovers <- withSystemFields (schema validRequest) resModel
  runRootResModel resovers validRequest

validateReq ::
  ( Monad m,
    ValidateSchema s
  ) =>
  Config ->
  Schema s ->
  GQLRequest ->
  ResponseStream event m ResolverContext
validateReq config inputSchema request = cleanEvents $ ResultT $ pure $ do
  validSchema <- validateSchema True config inputSchema
  schema <- internalSchema <:> validSchema
  operation <- parseRequestWith config schema request
  pure $
    ResolverContext
      { schema,
        config,
        operation,
        currentTypeName = "Root",
        currentSelection =
          Selection
            { selectionName = "Root",
              selectionArguments = empty,
              selectionPosition = operationPosition operation,
              selectionAlias = Nothing,
              selectionContent = SelectionSet (operationSelection operation),
              selectionDirectives = []
            }
      }

parseDSL :: ByteString -> Either String (Schema VALID)
parseDSL = resultOr (Left . show) pure . parseGQLDocument

parseGQLDocument :: ByteString -> Eventless (Schema VALID)
parseGQLDocument = parseTypeSystemDefinition . LT.toStrict . decodeUtf8

parseFullGQLDocument :: ByteString -> Eventless (Schema VALID)
parseFullGQLDocument = parseGQLDocument >=> (internalSchema <:>)
