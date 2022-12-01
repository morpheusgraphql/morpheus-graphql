{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Morpheus.App.Internal.Resolving.MonadResolver
import Data.Morpheus.App.Internal.Resolving.ResolveValue
import Data.Morpheus.App.Internal.Resolving.Resolver
  ( Resolver,
    ResponseStream,
  )
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverState,
    toResolverStateT,
  )
import Data.Morpheus.App.Internal.Resolving.SchemaAPI (schemaAPI)
import Data.Morpheus.App.Internal.Resolving.Types
import Data.Morpheus.App.Internal.Resolving.Utils
  ( lookupResJSON,
  )
import Data.Morpheus.Internal.Ext (merge)
import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    MUTATION,
    Operation (..),
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    Selection,
    SelectionContent (SelectionSet),
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

rootResolver :: (MonadResolver m) => ResolverState (ObjectTypeResolver m) -> SelectionSet VALID -> m ValidValue
rootResolver res selection = do
  root <- liftState (toResolverStateT res)
  resolveObject (ResolverMapContext mempty mempty) root (Just selection)

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
      selectByOperation OPERATION_QUERY =
        runResolver channelMap (withIntrospection (rootResolver queryResolver) ctx) ctx
      selectByOperation OPERATION_MUTATION =
        runResolver channelMap (rootResolver mutationResolver operationSelection) ctx
      selectByOperation OPERATION_SUBSCRIPTION =
        runResolver channelMap (rootResolver subscriptionResolver operationSelection) ctx
runRootResolverValue
  NamedResolversValue {queryResolverMap}
  ctx@ResolverContext {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation OPERATION_QUERY = runResolver Nothing (withIntrospection resolvedValue ctx) ctx
        where
          resolvedValue selection = resolveRef (ResolverMapContext empty queryResolverMap) (NamedResolverRef "Query" ["ROOT"]) (SelectionSet selection)
      selectByOperation _ = throwError "mutation and subscription is not supported for namedResolvers"

withIntrospection :: (MonadResolver m, MonadOperation m ~ QUERY) => (SelectionSet VALID -> m ValidValue) -> ResolverContext -> m ValidValue
withIntrospection m ResolverContext {operation, schema} = case splitSystemSelection (operationSelection operation) of
  (Nothing, _) -> m $ operationSelection operation
  (Just intro, Nothing) -> resolveObject resMap (schemaAPI schema) (Just intro)
  (Just intro, Just selection) -> do
    x <- m selection
    y <- resolveObject resMap (schemaAPI schema) (Just intro)
    mergeRoot y x
  where
    resMap = ResolverMapContext mempty mempty

mergeRoot :: MonadError GQLError m => ValidValue -> ValidValue -> m ValidValue
mergeRoot (Object x) (Object y) = Object <$> merge x y
mergeRoot _ _ = throwError (internal "can't merge non object types")
