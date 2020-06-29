{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Morpheus.Server.Deriving.Resolve
  ( statelessResolver,
    RootResolverConstraint,
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
import Data.Morpheus.Internal.Utils (resolveUpdates)
import Data.Morpheus.Server.Deriving.Channels (ChannelCon)
import Data.Morpheus.Server.Deriving.Encode
  ( EncodeCon,
    deriveModel,
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( IntroCon,
    introspectObjectFields,
  )
import Data.Morpheus.Server.Types.GQLType (GQLType (CUSTOM))
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldsDefinition,
    MUTATION,
    OUT,
    QUERY,
    SUBSCRIPTION,
    Schema (..),
    TypeContent (..),
    TypeDefinition,
    TypeName,
    ValidValue,
    initTypeLib,
    mkType,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    GQLChannel (..),
    Resolver,
    ResponseStream,
    ResultT (..),
    RootResModel (channelMap),
    cleanEvents,
  )
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Debug.Trace

type EventCon event =
  (Eq (StreamChannel event), Typeable event, GQLChannel event)

type IntrospectConstraint m event query mutation subscription =
  ( IntroCon (query (Resolver QUERY event m)),
    IntroCon (mutation (Resolver MUTATION event m)),
    IntroCon (subscription (Resolver SUBSCRIPTION event m))
  )

type OperationConstraint operation event m a =
  ( EncodeCon operation event m (a (Resolver operation event m)),
    IntroCon (a (Resolver operation event m))
  )

type RootResolverConstraint m event query mutation subscription =
  ( EventCon event,
    Typeable m,
    Monad m,
    Show (StreamChannel event),
    OperationConstraint QUERY event m query,
    OperationConstraint MUTATION event m mutation,
    OperationConstraint SUBSCRIPTION event m subscription,
    ChannelCon event m subscription
  )

statelessResolver ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  GQLRequest ->
  m GQLResponse
statelessResolver root req =
  renderResponse <$> runResultT (coreResolver root req)

coreResolver ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
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
    execOperator schema = runApi schema (traceShow (channelMap (deriveModel root)) (deriveModel root)) request

fullSchema ::
  forall proxy m event query mutation subscription.
  (IntrospectConstraint m event query mutation subscription) =>
  proxy (RootResolver m event query mutation subscription) ->
  Eventless Schema
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
  where
    querySchema =
      resolveUpdates (initTypeLib (operatorType fields "Query")) types
      where
        (fields, types) =
          introspectObjectFields
            (Proxy @(CUSTOM (query (Resolver QUERY event m))))
            ("type for query", Proxy @(query (Resolver QUERY event m)))
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
              Proxy @(subscription (Resolver SUBSCRIPTION event m))
            )
    maybeOperator :: FieldsDefinition OUT -> TypeName -> Maybe (TypeDefinition OUT)
    maybeOperator fields
      | null fields = const Nothing
      | otherwise = Just . operatorType fields
    -------------------------------------------------
    operatorType :: FieldsDefinition OUT -> TypeName -> TypeDefinition OUT
    operatorType fields typeName = mkType typeName (DataObject [] fields)
