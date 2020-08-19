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
    deriveApp,
  )
where

import Data.Functor.Identity (Identity (..))
-- MORPHEUS

import Data.Morpheus.Core
  ( App (..),
    Config,
    runAppWithConfig,
  )
import Data.Morpheus.Internal.Utils (Failure)
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
  ( GQLErrors,
    MUTATION,
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
  Config ->
  GQLRequest ->
  m GQLResponse
statelessResolver root config req =
  renderResponse <$> runResultT (coreResolver root config req)

coreResolver ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  Config ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
coreResolver root config request = do
  app <- deriveApp root
  runAppWithConfig app config request

deriveApp ::
  ( Functor f,
    Failure GQLErrors f,
    RootResolverConstraint m event query mut sub
  ) =>
  RootResolver m event query mut sub ->
  f (App event m)
deriveApp root = App (deriveModel root) <$> deriveSchema (Identity root)
