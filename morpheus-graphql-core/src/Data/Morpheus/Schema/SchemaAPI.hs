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
  ( Argument (..),
    OUT,
    QUERY,
    ScalarValue (..),
    Schema (..),
    TypeDefinition (..),
    Value (..),
  )
import Data.Morpheus.Types.Internal.Operation
  ( Merge (..),
    selectOr,
    toList,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ObjectResModel (..),
    ResModel (..),
    Resolver,
    ResultT,
    RootResModel (..),
    withArguments,
  )
import Data.Text (Text)

resolveTypes ::
  Monad m => Schema -> Resolver QUERY e m (ResModel QUERY e m)
resolveTypes schema = ResList <$> traverse (`render` schema) (toList schema)

buildSchemaLinkType ::
  Monad m => Maybe (TypeDefinition OUT) -> ResModel QUERY e m
buildSchemaLinkType (Just TypeDefinition {typeName}) = createObjectType typeName Nothing [] []
buildSchemaLinkType Nothing = ResNull

findType ::
  Monad m =>
  Text ->
  Schema ->
  Resolver QUERY e m (ResModel QUERY e m)
findType name schema = selectOr (pure ResNull) (`render` schema) name schema

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
            [ ("__type", withArguments typeResolver),
              ("__schema", schemaResolver schema)
            ]
        }
    )
  where
    typeResolver = selectOr (pure ResNull) handleArg "name"
      where
        handleArg
          Argument
            { argumentValue = (Scalar (String typename))
            } = findType typename schema
        handleArg _ = pure ResNull

withSystemFields :: Monad m => Schema -> RootResModel e m -> ResultT e' m (RootResModel e m)
withSystemFields schema RootResModel {query, ..} =
  pure $
    RootResModel
      { query = query >>= (<:> schemaAPI schema),
        ..
      }
