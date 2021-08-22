{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.TypeResolvers
  ( TypeResolvers,
    runRootTypeResolver,
    mkTypeResolver,
    mkTypeResolverRef,
    mkTypeResolvers,
    RelTypeResolver,
    TypeResolver (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Internal as HM
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
  ( EncoderContext (..),
    ObjectTypeResolver (..),
    ResolverValueDefinition (..),
    resolveObjectTypeResolver,
    resolveResolverDefinition,
  )
import Data.Morpheus.Internal.Utils (KeyOf (keyOf), selectOr)
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    Operation (..),
    Selection (..),
    SelectionSet,
    TypeName,
    TypeName,
    VALID,
    ValidValue,
    Value (..),
  )
import Relude

type RelTypeResolver o e m =
  ValidValue ->
  [(FieldName, Resolver o e m RelationalResolver)]

mkTypeResolver :: TypeName -> [(FieldName, RelationalResolver)] -> ObjectTypeResolver RelationalResolver
mkTypeResolver typeName = ObjectTypeResolver typeName . HM.fromList

mkTypeResolverRef :: TypeName -> ValidValue -> RelationalResolver
mkTypeResolverRef typeName = ResObject . TypeResolverRef typeName

mkTypeResolvers :: (LiftOperation o, Monad m) => [(TypeName, RelTypeResolver o e m)] -> TypeResolvers (Resolver o e m)
mkTypeResolvers = HM.fromList . map packRes
  where
    packRes :: Applicative m => (TypeName, ValidValue -> [(FieldName, m RelationalResolver)]) -> (TypeName, TypeResolver m)
    packRes (typeName, value) =
      ( typeName,
        TypeResolver
          typeName
          ( pure
              . (ObjectTypeResolver typeName . HM.fromList)
              . value
          )
      )

type TypeResolvers (m :: * -> *) = HashMap TypeName (TypeResolver m)

data TypeResolver m = TypeResolver
  { resolverName :: TypeName,
    resolver :: ValidValue -> m (ObjectTypeResolver (m RelationalResolver))
  }

instance KeyOf TypeName (TypeResolver m) where
  keyOf = resolverName

type RelationalResolver = ResolverValueDefinition () TypeResolverRef

data TypeResolverRef = TypeResolverRef
  { typeName :: TypeName,
    typeId :: ValidValue
  }
  deriving (Show)

runRootTypeResolver ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  TypeName ->
  ResolverContext ->
  TypeResolvers (Resolver o e m) ->
  ResponseStream e m (Value VALID)
runRootTypeResolver
  channels
  resolverName
  ctx@ResolverContext {operation = Operation {operationSelection}}
  res = runResolver channels resolvedValue ctx
    where
      resolvedValue = resolveRef res (TypeResolverRef resolverName Null) operationSelection

resolveRef ::
  ( MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
  TypeResolvers m ->
  TypeResolverRef ->
  SelectionSet VALID ->
  m ValidValue
resolveRef resolvers ref selection = do
  objectResolver <- selectOr cantFoundError ((typeId ref &) . resolver) (typeName ref) resolvers
  let resolveValue =
        resolveResolverDefinition
          EncoderContext
            { encodeUnion = \_ _ -> pure Null, --TODO: real union
              getTypeName = typeName,
              mkEnumUnion = (`ResUnion` ()),
              encodeObject = resolveRef resolvers
            }
  resolveObjectTypeResolver (>>= resolveValue) objectResolver selection
  where
    cantFoundError = throwError "Resolver Type can't found"
