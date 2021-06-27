{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.RootResolverValue
  ( runRootResolverValue,
    RootResolverValue (..),
  )
where

import qualified Data.Aeson as A
import Data.Morpheus.App.Internal.Resolving.Event
  ( EventHandler (..),
  )
import Data.Morpheus.App.Internal.Resolving.Resolver
  ( LiftOperation,
    Resolver,
    ResponseStream,
    runResolver,
  )
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverState,
    runResolverStateT,
    toResolverStateT,
  )
import Data.Morpheus.App.Internal.Resolving.ResolverValue
  ( ResolverValue (..),
    mkValue,
    resolveObject,
  )
import Data.Morpheus.Internal.Utils
  ( selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    Operation (..),
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    Selection,
    VALID,
    Value (..),
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

instance Monad m => A.FromJSON (RootResolverValue e m) where
  parseJSON res =
    pure
      RootResolverValue
        { query = lookupRes "query" res,
          mutation = lookupRes "mutation" res,
          subscription = lookupRes "subscription" res,
          channelMap = Nothing
        }

lookupRes :: (Monad m, Monad m') => Text -> A.Value -> m (ResolverValue m')
lookupRes name (A.Object fields) = pure (selectOr ResNull mkValue name fields)
lookupRes _ _ = pure ResNull

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
