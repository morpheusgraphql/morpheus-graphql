{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Schema.SchemaAPI
  ( withSystemFields,
  )
where

-- MORPHEUS
import Data.Morpheus.Internal.Utils
  ( (<:>),
    elems,
    empty,
    selectOr,
  )
import Data.Morpheus.Rendering.RenderIntrospection
  ( createObjectType,
    render,
  )
import Data.Morpheus.Schema.Directives
  ( defaultDirectives,
  )
import Data.Morpheus.Schema.Schema
  (
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    OUT,
    QUERY,
    ScalarValue (..),
    Schema (..),
    TypeDefinition (..),
    TypeName (..),
    Value (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResModel,
    Resolver,
    ResultT,
    RootResModel (..),
    mkList,
    mkNull,
    mkObject,
    withArguments,
  )

resolveTypes ::
  Monad m => Schema -> Resolver QUERY e m (ResModel QUERY e m)
resolveTypes schema = mkList <$> traverse render (elems schema)

buildSchemaLinkType ::
  Monad m => Maybe (TypeDefinition OUT) -> Resolver QUERY e m (ResModel QUERY e m)
buildSchemaLinkType (Just TypeDefinition {typeName}) = createObjectType typeName Nothing [] empty
buildSchemaLinkType Nothing = pure mkNull

findType ::
  Monad m =>
  TypeName ->
  Schema ->
  Resolver QUERY e m (ResModel QUERY e m)
findType = selectOr (pure mkNull) render

renderDirectives ::
  Monad m =>
  Resolver QUERY e m (ResModel QUERY e m)
renderDirectives =
  mkList
    <$> traverse
      render
      defaultDirectives

schemaResolver ::
  Monad m =>
  Schema ->
  Resolver QUERY e m (ResModel QUERY e m)
schemaResolver schema@Schema {query, mutation, subscription} =
  pure $
    mkObject
      "__Schema"
      [ ("types", resolveTypes schema),
        ("queryType", buildSchemaLinkType (Just query)),
        ("mutationType", buildSchemaLinkType mutation),
        ("subscriptionType", buildSchemaLinkType subscription),
        ("directives", renderDirectives)
      ]

schemaAPI :: Monad m => Schema -> ResModel QUERY e m
schemaAPI schema =
  mkObject
    "Root"
    [ ("__type", withArguments typeResolver),
      ("__schema", schemaResolver schema)
    ]
  where
    typeResolver = selectOr (pure mkNull) handleArg "name"
      where
        handleArg
          Argument
            { argumentValue = (Scalar (String typename))
            } = findType (TypeName typename) schema
        handleArg _ = pure mkNull

withSystemFields :: Monad m => Schema -> RootResModel e m -> ResultT e' m (RootResModel e m)
withSystemFields schema RootResModel {query, ..} =
  pure $
    RootResModel
      { query = query >>= (<:> schemaAPI schema),
        ..
      }
