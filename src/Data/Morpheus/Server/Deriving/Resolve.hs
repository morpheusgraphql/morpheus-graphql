{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Morpheus.Server.Deriving.Resolve
  ( statelessResolver,
    RootResCon,
    fullSchema,
    coreResolver,
    EventCon,
  )
where

import Data.Functor.Identity (Identity (..))
-- MORPHEUS

import Data.Morpheus.Core
  ( runApi,
  )
import Data.Morpheus.Server.Deriving.Encode
  ( EncodeCon,
    deriveModel,
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( IntroCon,
    TypeScope (..),
    introspectObjectFields,
  )
import Data.Morpheus.Server.Types.GQLType (GQLType (CUSTOM))
import Data.Morpheus.Types
  ( GQLRootResolver (..),
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
    OUT,
    QUERY,
    SUBSCRIPTION,
    Schema (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    ValidValue,
    initTypeLib,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    GQLChannel (..),
    Resolver,
    ResponseStream,
    ResultT (..),
    cleanEvents,
    resolveUpdates,
  )
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)

type EventCon event =
  (Eq (StreamChannel event), Typeable event, GQLChannel event)

type IntrospectConstraint m event query mutation subscription =
  ( IntroCon (query (Resolver QUERY event m)),
    IntroCon (mutation (Resolver MUTATION event m)),
    IntroCon (subscription (Resolver SUBSCRIPTION event m))
  )

type RootResCon m event query mutation subscription =
  ( EventCon event,
    Typeable m,
    IntrospectConstraint m event query mutation subscription,
    EncodeCon QUERY event m (query (Resolver QUERY event m)),
    EncodeCon MUTATION event m (mutation (Resolver MUTATION event m)),
    EncodeCon
      SUBSCRIPTION
      event
      m
      (subscription (Resolver SUBSCRIPTION event m))
  )

statelessResolver ::
  (Monad m, RootResCon m event query mut sub) =>
  GQLRootResolver m event query mut sub ->
  GQLRequest ->
  m GQLResponse
statelessResolver root req =
  renderResponse <$> runResultT (coreResolver root req)

coreResolver ::
  forall event m query mut sub.
  (Monad m, RootResCon m event query mut sub) =>
  GQLRootResolver m event query mut sub ->
  GQLRequest ->
  ResponseStream event m ValidValue
coreResolver root request =
  validRequest
    >>= execOperator
  where
    validRequest ::
      Monad m => ResponseStream event m Schema
    validRequest = cleanEvents $ ResultT $ pure $ fullSchema $ Identity root
    --------------------------------------
    execOperator schema = runApi schema (deriveModel root) request

fullSchema ::
  forall proxy m event query mutation subscription.
  (IntrospectConstraint m event query mutation subscription) =>
  proxy (GQLRootResolver m event query mutation subscription) ->
  Eventless Schema
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
  where
    querySchema =
      resolveUpdates (initTypeLib (operatorType fields "Query")) types
      where
        (fields, types) =
          introspectObjectFields
            (Proxy @(CUSTOM (query (Resolver QUERY event m))))
            ("type for query", OutputType, Proxy @(query (Resolver QUERY event m)))
    ------------------------------
    mutationSchema lib =
      resolveUpdates
        (lib {mutation = maybeOperator fields "Mutation"})
        types
      where
        (fields, types) =
          introspectObjectFields
            (Proxy @(CUSTOM (mutation (Resolver MUTATION event m))))
            ( "type for mutation",
              OutputType,
              Proxy @(mutation (Resolver MUTATION event m))
            )
    ------------------------------
    subscriptionSchema lib =
      resolveUpdates
        (lib {subscription = maybeOperator fields "Subscription"})
        types
      where
        (fields, types) =
          introspectObjectFields
            (Proxy @(CUSTOM (subscription (Resolver SUBSCRIPTION event m))))
            ( "type for subscription",
              OutputType,
              Proxy @(subscription (Resolver SUBSCRIPTION event m))
            )
    maybeOperator :: FieldsDefinition -> TypeName -> Maybe (TypeDefinition OUT)
    maybeOperator (FieldsDefinition x) | null x = const Nothing
    maybeOperator fields = Just . operatorType fields
    -------------------------------------------------
    operatorType :: FieldsDefinition -> TypeName -> TypeDefinition OUT
    operatorType fields typeName =
      TypeDefinition
        { typeContent = DataObject [] fields,
          typeName,
          typeFingerprint = DataFingerprint typeName [],
          typeMeta = Nothing
        }
