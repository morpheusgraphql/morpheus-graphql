{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.SchemaAPI
  ( schemaAPI,
  )
where

import Data.Morpheus.App.Internal.Resolving.MonadResolver
  ( MonadResolver (..),
    ResolverContext (..),
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

resolveSchema :: (MonadResolver m) => Schema VALID -> m (ResolverValue m)
resolveSchema schema@Schema {..} =
  pure
    $ mkObject
      "__Schema"
      [ ("types", renderI $ toList $ typeDefinitions schema),
        ("queryType", renderI (Just query)),
        ("mutationType", renderI mutation),
        ("subscriptionType", renderI subscription),
        ("directives", renderI $ sortWith directiveDefinitionName $ toList directiveDefinitions)
      ]

resolveType :: (MonadResolver m) => Value VALID -> m (ResolverValue m)
resolveType (Scalar (String typename)) = asks (typeDefinitions . schema) >>= renderI . lookup (packName typename)
resolveType _ = pure mkNull

schemaAPI ::
  ( MonadOperation m ~ QUERY,
    MonadResolver m
  ) =>
  ObjectTypeResolver m
schemaAPI =
  ObjectTypeResolver
    ( fromList
        [ ("__type", getArgument "name" >>= resolveType),
          ("__schema", asks schema >>= resolveSchema)
        ]
    )
