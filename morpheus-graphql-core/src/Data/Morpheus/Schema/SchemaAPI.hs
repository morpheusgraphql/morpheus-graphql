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

import Data.Morpheus.Rendering.RenderIntrospection
  ( createObjectType,
    render,
  )
import Data.Morpheus.Schema.Schema
  (
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldsDefinition,
    Name,
    QUERY,
    ScalarValue (..),
    Schema (..),
    TypeDefinition (..),
    Value (..),
    allDataTypes,
    lookupDataType,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Merge (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ObjectResModel (..),
    ResModel (..),
    Resolver,
    ResultT,
    RootResModel (..),
  )
import Data.Text (Text)

resolveTypes ::
  Monad m => Schema -> Resolver QUERY e m (ResModel QUERY e m)
resolveTypes lib = ResList <$> traverse (`render` lib) (allDataTypes lib)

buildSchemaLinkType ::
  Monad m => Maybe TypeDefinition -> ResModel QUERY e m
buildSchemaLinkType (Just TypeDefinition {typeName}) = createObjectType typeName Nothing $ Just []
buildSchemaLinkType Nothing = ResNull

findType ::
  Monad m =>
  Text ->
  Schema ->
  Resolver QUERY e m (ResModel QUERY e m)
findType name lib = maybe (pure ResNull) (`render` lib) (lookupDataType name lib)

schemaResolver ::
  Monad m =>
  Schema ->
  Resolver QUERY e m (ResModel QUERY e m)
schemaResolver schema@Schema {query, mutation, subscription} =
  pure $
    ResObject
      ( ObjectResModel
          { __typename = "__Schema",
            objectFields =
              [ ("types", resolveTypes schema),
                ("queryType", pure $ buildSchemaLinkType $ Just query),
                ("mutationType", pure $ buildSchemaLinkType mutation),
                ("subscriptionType", pure $ buildSchemaLinkType subscription),
                ("directives", pure $ ResList [])
              ]
          }
      )

schemaAPI :: Monad m => Schema -> ResModel QUERY e m
schemaAPI schema =
  ResObject
    ( ObjectResModel
        { __typename = "Root",
          objectFields =
            [ ("__type", typeResolver),
              ("__schema", schemaResolver schema)
            ]
        }
    )
  where
    typeResolver = findType "__Schema" schema

withSystemFields :: Monad m => Schema -> RootResModel e m -> ResultT e' m (RootResModel e m)
withSystemFields schema RootResModel {query, ..} =
  pure $
    RootResModel
      { query = query >>= (<:> schemaAPI schema),
        ..
      }
