{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Schema.SchemaAPI
  ( withSystemFields,
  )
where

import Data.Morpheus.Ext.SemigroupM ((<:>))
import Data.Morpheus.Internal.Utils
  ( elems,
    empty,
    selectOr,
  )
import Data.Morpheus.Rendering.RenderIntrospection
  ( WithSchema,
    createObjectType,
    render,
  )
import Data.Morpheus.Schema.Schema
  (
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    FieldName,
    OBJECT,
    QUERY,
    ScalarValue (..),
    Schema (..),
    TypeDefinition (..),
    TypeName (..),
    VALID,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Resolver,
    ResolverValue,
    ResultT,
    RootResModel (..),
    mkList,
    mkNull,
    mkObject,
    withArguments,
  )
import Relude hiding (empty)

resolveTypes :: (Monad m, WithSchema m) => Schema VALID -> m (ResolverValue m)
resolveTypes schema = mkList <$> traverse render (elems schema)

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
findType = selectOr (pure mkNull) render

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
        ("directives", render directiveDefinitions)
      ]

schemaAPI :: Monad m => Schema VALID -> ResolverValue (Resolver QUERY e m)
schemaAPI schema =
  mkObject
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
            } = findType (TypeName typename) schema
        handleArg _ = pure mkNull

withSystemFields ::
  Monad m =>
  Schema VALID ->
  RootResModel e m ->
  ResultT e' m (RootResModel e m)
withSystemFields schema RootResModel {query, ..} =
  pure $
    RootResModel
      { query = query >>= (<:> schemaAPI schema),
        ..
      }
