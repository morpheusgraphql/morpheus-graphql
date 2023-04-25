{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.SchemaAPI
  ( schemaAPI,
  )
where

import Data.Morpheus.App.Internal.Resolving.MonadResolver
  ( MonadResolver (..),
    withArguments,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( ObjectTypeResolver (..),
    ResolverValue,
    mkNull,
    mkObject,
  )
import Data.Morpheus.App.RenderIntrospection
  ( renderI,
  )
import Data.Morpheus.Internal.Utils
  ( lookup,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    DirectiveDefinition (..),
    FieldName,
    QUERY,
    ScalarValue (..),
    Schema (..),
    VALID,
    Value (..),
    packName,
    typeDefinitions,
  )
import Relude hiding (empty)

schemaResolver ::
  MonadResolver m =>
  Schema VALID ->
  m (ResolverValue m)
schemaResolver schema@Schema {query, mutation, subscription, directiveDefinitions} =
  pure $
    mkObject
      "__Schema"
      [ ("types", renderI $ toList $ typeDefinitions schema),
        ("queryType", renderI (Just query)),
        ("mutationType", renderI mutation),
        ("subscriptionType", renderI subscription),
        ("directives", renderI $ sortWith directiveDefinitionName $ toList directiveDefinitions)
      ]

schemaAPI ::
  ( MonadOperation m ~ QUERY,
    MonadResolver m
  ) =>
  Schema VALID ->
  ObjectTypeResolver m
schemaAPI schema =
  ObjectTypeResolver
    ( fromList
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
            } = renderI $ lookup (packName typename) (typeDefinitions schema)
        handleArg _ = pure mkNull
