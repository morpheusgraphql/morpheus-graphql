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
import Data.Morpheus.App.Internal.Resolving.NamedResolver
  ( ResolverMap,
    runResolverMap
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
import Control.Monad.Except (throwError)

data RootResolverValue e m
  = RootResolverValue
      { queryResolver :: ResolverState (ResolverObject (Resolver QUERY e m)),
        mutationResolver :: ResolverState (ResolverObject (Resolver MUTATION e m)),
        subscriptionResolver :: ResolverState (ResolverObject (Resolver SUBSCRIPTION e m)),
        channelMap :: Maybe (Selection VALID -> ResolverState (Channel e))
      }
  | NamedResolvers
      { queryResolverMap :: ResolverMap (Resolver QUERY e m)
        -- TODO: implement later
        --    mutationResolverMap :: Maybe (ResolverMap (Resolver MUTATION e m)),
        --    subscriptionResolverMap :: Maybe (ResolverMap (Resolver SUBSCRIPTION e m)),
        -- typeResolverChannels :: Maybe (Selection VALID -> ResolverState (Channel e))
      }

instance Monad m => A.FromJSON (RootResolverValue e m) where
  parseJSON res =
    pure
      RootResolverValue
        { queryResolver = lookupResJSON "query" res,
          mutationResolver = lookupResJSON "mutation" res,
          subscriptionResolver = lookupResJSON "subscription" res,
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
    { queryResolver,
      mutationResolver,
      subscriptionResolver,
      channelMap
    }
  ctx@ResolverContext {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation Query =
        runRootDataResolver channelMap queryResolver ctx
      selectByOperation Mutation =
        runRootDataResolver channelMap mutationResolver ctx
      selectByOperation Subscription =
        runRootDataResolver channelMap subscriptionResolver ctx
runRootResolverValue
  NamedResolvers
    { queryResolverMap
      -- mutationResolverMap,
      --  subscriptionResolverMap,
      --typeResolverChannels
    }
  ctx@ResolverContext {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation Query =
        runResolverMap Nothing  "Query" ctx queryResolverMap
      -- TODO: support mutation and subscription
      selectByOperation _ = throwError "mutation and subscription is not yet supported"
      -- selectByOperation Mutation =
      --   runResolverMap typeResolverChannels "Mutation" ctx mutationResolverMap
      -- selectByOperation Subscription =
      --   runResolverMap typeResolverChannels "Subscription" ctx subscriptionResolverMap
