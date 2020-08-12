{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Schema.SchemaAPI
  ( withSystemFields,
  )
where

-- MORPHEUS
import Data.Morpheus.Internal.Utils
  ( (<.>),
    elems,
    empty,
    selectOr,
  )
import Data.Morpheus.Rendering.RenderIntrospection
  ( createObjectType,
    render,
  )
import Data.Morpheus.Schema.Schema
  (
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    FieldName,
    OUTPUT_OBJECT,
    QUERY,
    ScalarValue (..),
    Schema (..),
    TypeDefinition (..),
    TypeName (..),
    VALID,
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
  Monad m => Schema VALID -> Resolver QUERY e m (ResModel QUERY e m)
resolveTypes schema = mkList <$> traverse render (elems schema)

renderOperation ::
  Monad m =>
  Maybe (TypeDefinition OUTPUT_OBJECT VALID) ->
  Resolver QUERY e m (ResModel QUERY e m)
renderOperation (Just TypeDefinition {typeName}) = pure $ createObjectType typeName Nothing [] empty
renderOperation Nothing = pure mkNull

findType ::
  Monad m =>
  TypeName ->
  Schema VALID ->
  Resolver QUERY e m (ResModel QUERY e m)
findType = selectOr (pure mkNull) render

schemaResolver ::
  Monad m =>
  Schema VALID ->
  Resolver QUERY e m (ResModel QUERY e m)
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

schemaAPI :: Monad m => Schema VALID -> ResModel QUERY e m
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
      { query = query >>= (<.> schemaAPI schema),
        ..
      }
