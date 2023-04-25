{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
    Arguments,
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

resolveSchema ::
  MonadResolver m =>
  Schema VALID ->
  m (ResolverValue m)
resolveSchema schema@Schema {..} =
  pure $
    mkObject
      "__Schema"
      [ ("types", renderI $ toList $ typeDefinitions schema),
        ("queryType", renderI (Just query)),
        ("mutationType", renderI mutation),
        ("subscriptionType", renderI subscription),
        ("directives", renderI $ sortWith directiveDefinitionName $ toList directiveDefinitions)
      ]

resolveType :: MonadResolver m => Schema VALID -> Arguments VALID -> m (ResolverValue m)
resolveType schema = selectOr (pure mkNull) handleArg ("name" :: FieldName)
  where
    handleArg
      Argument
        { argumentValue = (Scalar (String typename))
        } = renderI $ lookup (packName typename) (typeDefinitions schema)
    handleArg _ = pure mkNull

schemaAPI ::
  ( MonadOperation m ~ QUERY,
    MonadResolver m
  ) =>
  Schema VALID ->
  ObjectTypeResolver m
schemaAPI schema =
  ObjectTypeResolver
    ( fromList
        [ ("__type", withArguments (resolveType schema)),
          ("__schema", resolveSchema schema)
        ]
    )
