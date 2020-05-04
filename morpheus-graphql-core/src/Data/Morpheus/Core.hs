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

-- import Data.Morpheus.Server.Schema.SchemaAPI
--   ( defaultTypes,
--     hiddenRootFields,
--     schemaAPI,
--   )

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
    GQLResponse (..),
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( DataFingerprint (..),
    FieldsDefinition (..),
    MUTATION,
    Name,
    Operation (..),
    QUERY,
    SUBSCRIPTION,
    Schema (..),
    Selection (..),
    SelectionContent (..),
    TypeContent (..),
    TypeDefinition (..),
    ValidValue,
    initTypeLib,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Merge (..),
    empty,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Context (..),
    Eventless,
    GQLChannel (..),
    GQLRootResolver (..),
    Resolver,
    ResolverModel,
    ResponseStream,
    Result (..),
    ResultT (..),
    cleanEvents,
    resolveUpdates,
    runResolverModel,
  )
import Data.Proxy (Proxy (..))
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
  ResponseStream event m ValidValue
runApi schema resModel request =
  validRequest >>= execOperator
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
    ----------------------------------------------------------
    execOperator ctx@Context {schema} = runResolverModel resModel ctx

parseDSL :: ByteString -> Either String Schema
parseDSL doc = case parseGraphQLDocument doc of
  Failure errors -> Left (show errors)
  Success {result} -> Right result

parseGraphQLDocument :: ByteString -> Eventless Schema
parseGraphQLDocument x = parseTypeSystemDefinition (LT.toStrict $ decodeUtf8 x)
