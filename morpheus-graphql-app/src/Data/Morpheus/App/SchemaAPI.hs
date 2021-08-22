{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.SchemaAPI
  ( withSystemFields,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    ResolverObject,
    ResolverValue,
    ResultT,
    RootResolverValue (..),
    mkList,
    mkNull,
    mkObject,
    mkObject',
    withArguments,
  )
import Data.Morpheus.App.RenderIntrospection
  ( WithSchema,
    createObjectType,
    render,
  )
import Data.Morpheus.Internal.Ext ((<:>))
import Data.Morpheus.Internal.Utils
  ( empty,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    FieldName,
    OBJECT,
    QUERY,
    ScalarValue (..),
    Schema (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    Value (..),
    packName,
    typeDefinitions,
  )
import Relude hiding (empty)

resolveTypes :: (Monad m, WithSchema m) => Schema VALID -> m (ResolverValue m)
resolveTypes schema = mkList <$> traverse render (toList $ typeDefinitions schema)

renderOperation ::
  (Monad m, WithSchema m) =>
  Maybe (TypeDefinition OBJECT VALID) ->
  m (ResolverValue m)
renderOperation (Just TypeDefinition {typeName}) = pure $ createObjectType typeName Nothing [] empty
renderOperation Nothing = pure mkNull

findType ::
  (Monad m, WithSchema m) =>
  TypeName ->
  Schema VALID ->
  m (ResolverValue m)
findType name = selectOr (pure mkNull) render name . typeDefinitions

schemaResolver ::
  (Monad m, WithSchema m) =>
  Schema VALID ->
  m (ResolverValue m)
schemaResolver schema@Schema {query, mutation, subscription, directiveDefinitions} =
  pure $
    mkObject
      "__Schema"
      [ ("types", resolveTypes schema),
        ("queryType", renderOperation (Just query)),
        ("mutationType", renderOperation mutation),
        ("subscriptionType", renderOperation subscription),
        ("directives", render $ toList directiveDefinitions)
      ]

schemaAPI :: Monad m => Schema VALID -> ResolverObject (Resolver QUERY e m)
schemaAPI schema =
  mkObject'
    "Root"
    [ ("__type", withArguments typeResolver),
      ("__schema", schemaResolver schema)
    ]
  where
    typeResolver = selectOr (pure mkNull) handleArg ("name" :: FieldName)
      where
        handleArg
          Argument
            { argumentValue = (Scalar (String typename))
            } = findType (packName typename) schema
        handleArg _ = pure mkNull

withSystemFields ::
  Monad m =>
  Schema VALID ->
  RootResolverValue e m ->
  ResultT e' m (RootResolverValue e m)
withSystemFields schema RootResolverValue {query, ..} =
  pure $
    RootResolverValue
      { query = query >>= (<:> schemaAPI schema),
        ..
      }
-- TODO: support introspection
withSystemFields schema TypeResolversValue {..} =
  pure $
    TypeResolversValue
      { ..
      }
