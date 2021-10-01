{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.NamedResolver
  ( ResolverMap,
    runResolverMap,
    NamedResolver (..),
    NamedResolverResult (..),
    NamedResolverField,
    NamedResolverRef (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving.Event (Channel)
import Data.Morpheus.App.Internal.Resolving.Resolver
  ( LiftOperation,
    Resolver,
    ResolverContext (..),
    ResponseStream,
    runResolver,
  )
import Data.Morpheus.App.Internal.Resolving.ResolverState (ResolverState)
import Data.Morpheus.App.Internal.Resolving.Utils
  ( NamedResolverRef (..),
    ObjectTypeResolver (..),
    ResolverValueDefinition (..),
    resolveObjectTypeResolver,
    resolveResolverDefinition,
  )
import Data.Morpheus.Internal.Utils (KeyOf (keyOf), selectOr)
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    Msg (msg),
    Selection (..),
    SelectionSet,
    TypeName,
    VALID,
    ValidValue,
    Value (Null),
  )
import Relude

type ResolverMap (m :: * -> *) = HashMap TypeName (NamedResolver m)

data NamedResolver (m :: * -> *) = NamedResolver
  { resolverName :: TypeName,
    resolver :: ValidValue -> m (NamedResolverResult m)
  }

data NamedResolverResult (m :: * -> *)
  = NamedObjectResolver (ObjectTypeResolver (m (NamedResolverField m)))
  | NamedUnionResolver NamedResolverRef
  | NamedEnumResolver TypeName

instance KeyOf TypeName (NamedResolver m) where
  keyOf = resolverName

type NamedResolverField = ResolverValueDefinition

runResolverMap ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  TypeName ->
  ResolverMap (Resolver o e m) ->
  ResolverContext ->
  SelectionSet VALID ->
  ResponseStream e m ValidValue
runResolverMap
  channels
  name
  res
  ctx
  selection = runResolver channels resolvedValue ctx
    where
      resolvedValue = resolveRef res (NamedResolverRef name Null) selection

resolveRef ::
  ( MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
  ResolverMap m ->
  NamedResolverRef ->
  SelectionSet VALID ->
  m ValidValue
resolveRef resolvers ref selection = do
  namedResolver <- getNamedResolverBy ref resolvers
  case namedResolver of
    NamedObjectResolver res -> resolveObjectTypeResolver (>>= resolveResolverDefinition) res selection

--  NamedUnionResolver unionRef -> resolveValue (ResUnion (resolverTypeName unionRef) (pure unionRef))
--   NamedEnumResolver enumValue -> resolveValue (ResEnum (Left enumValue))

getNamedResolverBy ::
  (MonadError GQLError m) =>
  NamedResolverRef ->
  ResolverMap m ->
  m (NamedResolverResult m)
getNamedResolverBy ref = selectOr cantFoundError ((resolverArgument ref &) . resolver) (resolverTypeName ref)
  where
    cantFoundError = throwError ("Resolver Type " <> msg (resolverTypeName ref) <> "can't found")
