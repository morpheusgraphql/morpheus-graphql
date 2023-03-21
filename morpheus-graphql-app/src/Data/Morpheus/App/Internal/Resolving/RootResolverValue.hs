{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.RootResolverValue
  ( runRootResolverValue,
    RootResolverValue (..),
  )
where

import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON (..))
import Data.HashMap.Strict (adjust)
import Data.Morpheus.App.Internal.Resolving.Batching
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
import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Types.Internal.AST
  ( MUTATION,
    Operation (..),
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    Schema (..),
    Selection,
    SelectionSet,
    TypeName,
    VALID,
    ValidValue,
    Value (..),
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

instance Monad m => FromJSON (RootResolverValue e m) where
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
  resolvePlainRoot root selection

runRootResolverValue :: Monad m => RootResolverValue e m -> ResolverContext -> ResponseStream e m (Value VALID)
runRootResolverValue
  RootResolverValue
    { queryResolver,
      mutationResolver,
      subscriptionResolver,
      channelMap
    }
  ctx@ResolverContext {operation = Operation {..}, ..} =
    selectByOperation operationType
    where
      selectByOperation OPERATION_QUERY =
        runResolver channelMap (rootResolver (withIntroFields schema <$> queryResolver) operationSelection) ctx
      selectByOperation OPERATION_MUTATION =
        runResolver channelMap (rootResolver mutationResolver operationSelection) ctx
      selectByOperation OPERATION_SUBSCRIPTION =
        runResolver channelMap (rootResolver subscriptionResolver operationSelection) ctx
runRootResolverValue
  NamedResolversValue {queryResolverMap}
  ctx@ResolverContext {operation = Operation {..}} =
    selectByOperation operationType
    where
      selectByOperation OPERATION_QUERY = runResolver Nothing queryResolver ctx
        where
          queryResolver = resolveNamedRoot (withNamedIntroFields "Query" ctx queryResolverMap) operationSelection
      selectByOperation _ = throwError "mutation and subscription is not supported for namedResolvers"

withNamedIntroFields :: (MonadResolver m, MonadOperation m ~ QUERY) => TypeName -> ResolverContext -> ResolverMap m -> ResolverMap m
withNamedIntroFields queryName ResolverContext {..} = adjust updateNamed queryName
  where
    updateNamed NamedResolver {..} = NamedResolver {resolverFun = const (updateResult <$> resolverFun ["ROOT"]), ..}
      where
        updateResult [NamedObjectResolver obj] = [NamedObjectResolver (withIntroFields schema obj)]
        updateResult value = value

withIntroFields :: (MonadResolver m, MonadOperation m ~ QUERY) => Schema VALID -> ObjectTypeResolver m -> ObjectTypeResolver m
withIntroFields schema (ObjectTypeResolver fields) = ObjectTypeResolver (fields <> objectFields (schemaAPI schema))
