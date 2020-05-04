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
  )
where

import Data.Functor.Identity (Identity (..))
-- MORPHEUS

import Data.Morpheus.Parsing.Internal
  ( parseRequestWith,
  )
-- import Data.Morpheus.Server.Schema.SchemaAPI
--   ( defaultTypes,
--     hiddenRootFields,
--     schemaAPI,
--   )
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
    ResultT (..),
    cleanEvents,
    resolveUpdates,
    runResolverModel,
  )
import Data.Proxy (Proxy (..))
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

-- TODO: add schema api without TH: (deriveModel root (schemaAPI schema))--
