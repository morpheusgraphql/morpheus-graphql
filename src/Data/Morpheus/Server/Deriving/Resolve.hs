{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Server.Deriving.Resolve
  ( statelessResolver,
    RootResolverConstraint,
    coreResolver,
    deriveSchema,
    deriveApp,
    stateless,
  )
where

import Data.Functor.Identity (Identity (..))
-- MORPHEUS

import Data.Morpheus.Core
  ( Api (..),
    ApiRunner (..),
    Config,
  )
import Data.Morpheus.Server.Deriving.Channels (ChannelCon)
import Data.Morpheus.Server.Deriving.Encode
  ( EncodeCon,
    deriveModel,
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( IntroCon,
    deriveSchema,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    QUERY,
    SUBSCRIPTION,
    VALID,
    Value,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Resolver,
    ResponseStream,
    Result (..),
    ResultT (..),
  )

type OperationConstraint operation event m a =
  ( EncodeCon operation event m (a (Resolver operation event m)),
    IntroCon (a (Resolver operation event m))
  )

type RootResolverConstraint m event query mutation subscription =
  ( Monad m,
    OperationConstraint QUERY event m query,
    OperationConstraint MUTATION event m mutation,
    OperationConstraint SUBSCRIPTION event m subscription,
    ChannelCon event m subscription
  )

statelessResolver ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  Config ->
  GQLRequest ->
  m GQLResponse
statelessResolver root config req = stateless (coreResolver root config req)

stateless ::
  Functor m =>
  ResponseStream event m (Value VALID) ->
  m GQLResponse
stateless = fmap renderResponse . runResultT

coreResolver ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  Config ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
coreResolver root = runAppWithConfig (deriveApp root)

deriveApp ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  App event m
deriveApp root = case deriveSchema (Identity root) of
  Success {result} -> App (deriveModel root) result
  Failure {errors} -> AppFailure errors
