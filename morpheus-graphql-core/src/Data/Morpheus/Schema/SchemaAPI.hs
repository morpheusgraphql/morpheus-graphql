{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Schema.SchemaAPI
  ( hiddenRootFields,
    schemaAPI,
  )
where

-- MORPHEUS

-- import Data.Morpheus.Server.Deriving.Introspect
--   ( TypeScope (..),
--   )
import Data.Morpheus.Rendering.RenderIntrospection
  ( createObjectType,
    render,
  )
import Data.Morpheus.Schema.Schema
  (
  )
import Data.Morpheus.Server.Types.GQLType (CUSTOM)
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
import Data.Morpheus.Types.Internal.Resolving
  ( ObjectResModel (..),
    ResModel (..),
    Resolver,
  )
import Data.Text (Text)

resolveTypes ::
  Monad m => Schema -> Resolver QUERY e m (ResModel QUERY e m)
resolveTypes lib = traverse (`render` lib) (allDataTypes lib)

buildSchemaLinkType ::
  Monad m => Maybe TypeDefinition -> ResModel QUERY e m
buildSchemaLinkType (Just TypeDefinition {typeName}) = createObjectType typeName Nothing $ Just []

findType ::
  Monad m =>
  Text ->
  Schema ->
  Resolver QUERY e m (ResModel QUERY e m)
findType name lib = maybe (pure ResNull) (render datatype lib) (lookupDataType name lib)

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
schemaResolver schema =
  ResObject
    ( ObjectResModel
        { __typename = "__Schema",
          objectFields =
            [ ("__SchemaTypes", resolveTypes schema),
              ("__SchemaQueryType", pure $ buildSchemaLinkType $ Just $ query schema),
              ("__SchemaMutationType", pure $ buildSchemaLinkType $ mutation schema),
              ("__SchemaSubscriptionType", pure $ buildSchemaLinkType $ subscription schema),
              ("__SchemaDirectives", pure $ ResList [])
            ]
        }
    )

hiddenRootFields :: FieldsDefinition
hiddenRootFields = undefined

-- fst $
--   introspectObjectFields
--     (Proxy :: Proxy (CUSTOM (Root Maybe)))
--     ("Root", OutputType, Proxy @(Root Maybe))

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
