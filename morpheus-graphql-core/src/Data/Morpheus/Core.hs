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
  )
where

-- MORPHEUS
import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Parsing.Internal
  ( parseRequestWith,
    parseTypeDefinitions,
    parseTypeSystemDefinition,
  )
import Data.Morpheus.Parsing.JSONSchema.Parse
  ( decodeIntrospection,
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
    ResolverModel,
    ResponseStream,
    ResultT (..),
    cleanEvents,
    resultOr,
    runResolverModel,
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
  ResolverModel event m ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
runApi schema resModel request =
  validRequest >>= runResolverModel resModel
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
