{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.RootResolverValue
  ( runRootResolverValue,
    RootResolverValue (..),
  )
where

import Control.Monad.Except (MonadError, throwError)
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
  ( lookupResJSON,
    resolveObject,
  )
import Data.Morpheus.App.Internal.Resolving.SchemaAPI (schemaAPI)
import Data.Morpheus.App.Internal.Resolving.Types
import Data.Morpheus.App.Internal.Resolving.Utils (runResolverMap)
import Data.Morpheus.Internal.Ext (merge)
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    MUTATION,
    Operation (..),
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    Selection,
    SelectionSet,
    VALID,
    ValidValue,
    Value (..),
    internal,
    splitSystemSelection,
  )
import Relude hiding
  ( Show,
    empty,
    show,
  )

data RootResolverValue e m
  = RootResolverValue
      { queryResolver :: ResolverState (ObjectTypeResolver (Resolver QUERY e m)),
        mutationResolver :: ResolverState (ObjectTypeResolver (Resolver MUTATION e m)),
        subscriptionResolver :: ResolverState (ObjectTypeResolver (Resolver SUBSCRIPTION e m)),
        channelMap :: Maybe (Selection VALID -> ResolverState (Channel e))
      }
  | NamedResolversValue
      {queryResolverMap :: ResolverMap (Resolver QUERY e m)}

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
  ResolverState (ObjectTypeResolver (Resolver o e m)) ->
  ResolverContext ->
  SelectionSet VALID ->
  ResponseStream e m (Value VALID)
runRootDataResolver
  channels
  res
  ctx
  selection =
    do
      root <- runResolverStateT (toResolverStateT res) ctx
      runResolver channels (resolveObject mempty root selection) ctx

runRootResolverValue :: Monad m => RootResolverValue e m -> ResolverContext -> ResponseStream e m (Value VALID)
runRootResolverValue
  RootResolverValue
    { queryResolver,
      mutationResolver,
      subscriptionResolver,
      channelMap
    }
  ctx@ResolverContext {operation = Operation {operationType, operationSelection}} =
    selectByOperation operationType
    where
      selectByOperation Query =
        withIntrospection (runRootDataResolver channelMap queryResolver) ctx
      selectByOperation Mutation =
        runRootDataResolver channelMap mutationResolver ctx operationSelection
      selectByOperation Subscription =
        runRootDataResolver channelMap subscriptionResolver ctx operationSelection
runRootResolverValue
  NamedResolversValue
    { queryResolverMap
      -- mutationResolverMap,
      -- subscriptionResolverMap,
      --typeResolverChannels
    }
  ctx@ResolverContext {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation Query = withIntrospection (runResolverMap Nothing "Query" queryResolverMap) ctx
      -- TODO: support mutation and subscription
      -- selectByOperation Mutation =
      --   runResolverMap typeResolverChannels "Mutation" ctx mutationResolverMap
      -- selectByOperation Subscription =
      --   runResolverMap typeResolverChannels "Subscription" ctx subscriptionResolverMap
      selectByOperation _ = throwError "mutation and subscription is not yet supported"

withIntrospection :: Monad m => (ResolverContext -> SelectionSet VALID -> ResponseStream event m ValidValue) -> ResolverContext -> ResponseStream event m ValidValue
withIntrospection f ctx@ResolverContext {operation} = case splitSystemSelection (operationSelection operation) of
  (Nothing, _) -> f ctx (operationSelection operation)
  (Just intro, Nothing) -> introspection intro ctx
  (Just intro, Just selection) -> do
    x <- f ctx selection
    y <- introspection intro ctx
    mergeRoot y x

introspection :: Monad m => SelectionSet VALID -> ResolverContext -> ResponseStream event m ValidValue
introspection selection ctx@ResolverContext {schema} = runResolver Nothing (resolveObject mempty (schemaAPI schema) selection) ctx

mergeRoot :: MonadError GQLError m => ValidValue -> ValidValue -> m ValidValue
mergeRoot (Object x) (Object y) = Object <$> merge x y
mergeRoot _ _ = throwError (internal "can't merge non object types")
