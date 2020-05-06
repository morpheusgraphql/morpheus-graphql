{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Core
  ( runApi,
    EventCon,
    parseDSL,
    parseGraphQLDocument,
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
runApi schema resModel request =
  validRequest >>= runRootResModel resModel
  where
    validRequest ::
      Monad m => ResponseStream event m Context
    validRequest = cleanEvents $ ResultT $ pure $ do
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
parseDSL = resultOr (Left . show) pure . parseGraphQLDocument

parseGraphQLDocument :: ByteString -> Eventless Schema
parseGraphQLDocument = parseTypeSystemDefinition . LT.toStrict . decodeUtf8
