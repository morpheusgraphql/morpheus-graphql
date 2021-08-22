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
  ( ResolverObject,
    lookupResJSON,
    resolveObject,
  )
import Data.Morpheus.App.Internal.Resolving.TypeResolvers
  ( TypeResolvers,
    runRootTypeResolver,
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

data RootResolverValue e m
  = RootResolverValue
      { query :: ResolverState (ResolverObject (Resolver QUERY e m)),
        mutation :: ResolverState (ResolverObject (Resolver MUTATION e m)),
        subscription :: ResolverState (ResolverObject (Resolver SUBSCRIPTION e m)),
        channelMap :: Maybe (Selection VALID -> ResolverState (Channel e))
      }
  | TypeResolversValue
      { queryTypeResolvers :: TypeResolvers (Resolver QUERY e m),
        mutationTypeResolvers :: Maybe (TypeResolvers (Resolver MUTATION e m)),
        subscriptionTypeResolvers :: Maybe (TypeResolvers (Resolver SUBSCRIPTION e m)),
        typeResolverChannels :: Maybe (Selection VALID -> ResolverState (Channel e))
      }

instance Monad m => A.FromJSON (RootResolverValue e m) where
  parseJSON res =
    pure
      RootResolverValue
        { query = lookupResJSON "query" res,
          mutation = lookupResJSON "mutation" res,
          subscription = lookupResJSON "subscription" res,
          channelMap = Nothing
        }

runRootDataResolver ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  ResolverState (ResolverObject (Resolver o e m)) ->
  ResolverContext ->
  ResponseStream e m (Value VALID)
runRootDataResolver
  channels
  res
  ctx@ResolverContext {operation = Operation {operationSelection}} =
    do
      root <- runResolverStateT (toResolverStateT res) ctx
      runResolver channels (resolveObject root operationSelection) ctx

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
runRootResolverValue
  TypeResolversValue
    { queryTypeResolvers,
      mutationTypeResolvers,
      subscriptionTypeResolvers,
      typeResolverChannels
    }
  ctx@ResolverContext {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation Query =
        runRootTypeResolver typeResolverChannels "Query" ctx queryTypeResolvers

-- selectByOperation Mutation =
--   runRootTypeResolver typeResolverChannels "Mutation" ctx mutationTypeResolvers
-- selectByOperation Subscription =
--   runRootTypeResolver typeResolverChannels "Subscription" ctx subscriptionTypeResolvers
