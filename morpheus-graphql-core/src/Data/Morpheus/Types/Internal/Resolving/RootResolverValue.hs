{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving.RootResolverValue
  ( runRootResolverValue,
    RootResolverValue (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    Operation (..),
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    Selection,
    VALID,
    Value,
  )
import Data.Morpheus.Types.Internal.Resolving.Event
  ( EventHandler (..),
  )
import Data.Morpheus.Types.Internal.Resolving.Resolver
  ( LiftOperation,
    Resolver,
    ResponseStream,
    runResolver,
  )
import Data.Morpheus.Types.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverState,
    runResolverStateT,
    toResolverStateT,
  )
import Data.Morpheus.Types.Internal.Resolving.ResolverValue
  ( ResolverValue (..),
    resolveObject,
  )
import Relude hiding
  ( Show,
    empty,
    show,
  )

data RootResolverValue e m = RootResolverValue
  { query :: ResolverState (ResolverValue (Resolver QUERY e m)),
    mutation :: ResolverState (ResolverValue (Resolver MUTATION e m)),
    subscription :: ResolverState (ResolverValue (Resolver SUBSCRIPTION e m)),
    channelMap :: Maybe (Selection VALID -> ResolverState (Channel e))
  }

runRootDataResolver ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  ResolverState (ResolverValue (Resolver o e m)) ->
  ResolverContext ->
  ResponseStream e m (Value VALID)
runRootDataResolver
  channels
  res
  ctx@ResolverContext {operation = Operation {operationSelection}} =
    do
      root <- runResolverStateT (toResolverStateT res) ctx
      runResolver channels (resolveObject operationSelection root) ctx

runRootResolverValue :: Monad m => RootResolverValue e m -> ResolverContext -> ResponseStream e m (Value VALID)
runRootResolverValue
  RootResolverValue
    { query,
      mutation,
      subscription,
      channelMap
    }
  ctx@ResolverContext {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation Query =
        runRootDataResolver channelMap query ctx
      selectByOperation Mutation =
        runRootDataResolver channelMap mutation ctx
      selectByOperation Subscription =
        runRootDataResolver channelMap subscription ctx
