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
  ( Eventless,
    ObjectResModel (..),
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

findType ::
  Monad m =>
  Text ->
  Schema ->
  Resolver QUERY e m (ResModel QUERY e m)
findType name lib = maybe (pure ResNull) (`render` lib) (lookupDataType name lib)

-- pure
--   $ ResObject
--   $ ObjectResModel
--     { __typename = "Deity",
--       objectFields =
--         [ ( "name",
--             pure $ string "Morpheus"
--           ),
--           ( "power",
--             pure $
--               ResList
--                 [string "Shapeshifting"]
--           )
--         ]
--     }

schemaResolver ::
  Monad m =>
  Schema ->
  ResModel QUERY e m
schemaResolver schema@Schema {query, mutation, subscription} =
  ResObject
    ( ObjectResModel
        { __typename = "__Schema",
          objectFields =
            [ ("__SchemaTypes", resolveTypes schema),
              ("__SchemaQueryType", pure $ buildSchemaLinkType $ Just query),
              ("__SchemaMutationType", pure $ buildSchemaLinkType mutation),
              ("__SchemaSubscriptionType", pure $ buildSchemaLinkType subscription),
              ("__SchemaDirectives", pure $ ResList [])
            ]
        }
    )

string :: Name -> ResModel o e m
string = ResScalar . String

schemaAPI :: Monad m => Schema -> ResModel QUERY e m
schemaAPI schema =
  ResObject
    ( ObjectResModel
        { __typename = "Root",
          objectFields =
            [ ("__type", typeResolver),
              ("__schema", pure $ schemaResolver schema)
            ]
        }
    )
  where
    typeResolver = findType "name" schema

withSystemFields :: Monad m => Schema -> RootResModel e m -> ResultT e' m (RootResModel e m)
withSystemFields schema RootResModel {query, ..} =
  pure $
    RootResModel
      { query = query >>= (<:> schemaAPI schema),
        ..
      }
