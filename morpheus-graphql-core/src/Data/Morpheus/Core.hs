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
    EventCon,
    parseDSL,
    parseFullGQLDocument,
    parseGQLDocument,
    decodeIntrospection,
    parseTypeSystemDefinition,
    parseTypeDefinitions,
    validateRequest,
    parseRequestWith,
    parseRequest,
    RenderGQL (..),
  )
where

-- MORPHEUS
import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseRequestWith,
    parseTypeDefinitions,
    parseTypeSystemDefinition,
  )
import Data.Morpheus.Parsing.JSONSchema.Parse
  ( decodeIntrospection,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Schema.Schema (withSystemTypes)
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
import Data.Morpheus.Types.Internal.Operation
  ( empty,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Context (..),
    Eventless,
    GQLChannel (..),
    ResponseStream,
    ResultT (..),
    RootResModel,
    cleanEvents,
    resultOr,
    runRootResModel,
  )
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import qualified Data.Text.Lazy as LT
  ( toStrict,
  )
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable (Typeable)

type EventCon event =
  (Eq (StreamChannel event), Typeable event, GQLChannel event)

runApi ::
  forall event m.
  (Monad m) =>
  Schema ->
  RootResModel event m ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
runApi inputSchema resModel request =
  validRequest >>= runRootResModel resModel
  where
    validRequest ::
      Monad m => ResponseStream event m Context
    validRequest = cleanEvents $ ResultT $ pure $ do
      schema <- withSystemTypes inputSchema
      operation <- parseRequestWith schema request
      pure $
        Context
          { schema,
            operation,
            currentTypeName = "Root",
            currentSelection =
              Selection
                { selectionName = "Root",
                  selectionArguments = empty,
                  selectionPosition = operationPosition operation,
                  selectionAlias = Nothing,
                  selectionContent = SelectionSet (operationSelection operation)
                }
          }

parseDSL :: ByteString -> Either String Schema
parseDSL = resultOr (Left . show) pure . parseGQLDocument

parseGQLDocument :: ByteString -> Eventless Schema
parseGQLDocument = parseTypeSystemDefinition . LT.toStrict . decodeUtf8

parseFullGQLDocument :: ByteString -> Eventless Schema
parseFullGQLDocument = parseGQLDocument >=> withSystemTypes
