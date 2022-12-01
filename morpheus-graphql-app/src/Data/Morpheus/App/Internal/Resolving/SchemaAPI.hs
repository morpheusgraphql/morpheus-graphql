{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.SchemaAPI
  ( schemaAPI,
    schemaAPINamed,
  )
where

import Data.Morpheus.App.Internal.Resolving.MonadResolver
  ( MonadResolver (..),
    withArguments,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (NamedResolver, resolverFun, resolverName),
    NamedResolverResult (NamedObjectResolver),
    ObjectTypeResolver (..),
    ResolverMap,
    ResolverValue,
    mkList,
    mkNull,
    mkObject,
  )
import Data.Morpheus.App.RenderIntrospection
  ( createObjectType,
    render,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    DirectiveDefinition (..),
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
import qualified Relude as HM

resolveTypes :: MonadResolver m => Schema VALID -> m (ResolverValue m)
resolveTypes schema = mkList <$> traverse render (toList $ typeDefinitions schema)

renderOperation ::
  MonadResolver m =>
  Maybe (TypeDefinition OBJECT VALID) ->
  m (ResolverValue m)
renderOperation (Just TypeDefinition {typeName}) = pure $ createObjectType typeName Nothing [] empty
renderOperation Nothing = pure mkNull

findType ::
  MonadResolver m =>
  TypeName ->
  Schema VALID ->
  m (ResolverValue m)
findType name = selectOr (pure mkNull) render name . typeDefinitions

schemaResolver ::
  MonadResolver m =>
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
        ("directives", render $ sortWith directiveDefinitionName $ toList directiveDefinitions)
      ]

schemaAPINamed ::
  ( MonadOperation m ~ QUERY,
    MonadResolver m
  ) =>
  Schema VALID ->
  ResolverMap m
schemaAPINamed schema =
  HM.fromList
    [ ( "Query",
        NamedResolver
          { resolverName = "Query",
            resolverFun = const $ pure [NamedObjectResolver (schemaAPI schema)]
          }
      )
    ]

schemaAPI ::
  ( MonadOperation m ~ QUERY,
    MonadResolver m
  ) =>
  Schema VALID ->
  ObjectTypeResolver m
schemaAPI schema =
  ObjectTypeResolver
    ( HM.fromList
        [ ("__type", withArguments typeResolver),
          ("__schema", schemaResolver schema)
        ]
    )
  where
    typeResolver = selectOr (pure mkNull) handleArg ("name" :: FieldName)
      where
        handleArg
          Argument
            { argumentValue = (Scalar (String typename))
            } = findType (packName typename) schema
        handleArg _ = pure mkNull
