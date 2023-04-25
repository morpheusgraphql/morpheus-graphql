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
    ResolverContext (schema),
    getArgument,
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
  ( IsMap (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveDefinition (..),
    QUERY,
    ScalarValue (..),
    Schema (..),
    VALID,
    Value (..),
    packName,
    typeDefinitions,
  )
import Relude hiding (empty)

resolveSchema :: MonadResolver m => Schema VALID -> m (ResolverValue m)
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

resolveType :: MonadResolver m => Value VALID -> m (ResolverValue m)
resolveType (Scalar (String typename)) = asks schema >>= renderI . lookup (packName typename) . typeDefinitions
resolveType _ = pure mkNull

schemaAPI ::
  ( MonadOperation m ~ QUERY,
    MonadResolver m
  ) =>
  Schema VALID ->
  ObjectTypeResolver m
schemaAPI schema =
  ObjectTypeResolver
    ( fromList
        [ ("__type", getArgument "name" >>= resolveType),
          ("__schema", resolveSchema schema)
        ]
    )
