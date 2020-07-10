{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Server.Deriving.Resolve
  ( statelessResolver,
    RootResolverConstraint,
    coreResolver,
    deriveSchema,
  )
where

import Data.Functor.Identity (Identity (..))
-- MORPHEUS

import Data.Morpheus.Core
  ( runApi,
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
  GQLRequest ->
  m GQLResponse
statelessResolver root req =
  renderResponse <$> runResultT (coreResolver root req)

coreResolver ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
coreResolver root request = do
  schema <- deriveSchema (Identity root)
  runApi schema (deriveModel root) request
