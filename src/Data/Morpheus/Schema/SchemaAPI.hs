{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.SchemaAPI
  ( hiddenRootFields
  , defaultTypes
  , schemaAPI
  )
where

import           Data.Proxy
import           Data.Text                      ( Text )

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect
                                                ( introspectObjectFields
                                                , TypeUpdater
                                                , introspect
                                                , TypeScope(..)
                                                )
import           Data.Morpheus.Rendering.RenderIntrospection
                                                ( createObjectType
                                                , render
                                                )
import           Data.Morpheus.Schema.Schema    ( Root(..)
                                                , Root__typeArgs(..)
                                                , S__Schema(..)
                                                , S__Type
                                                )
import           Data.Morpheus.Types.GQLType    ( CUSTOM )
import           Data.Morpheus.Types.ID         ( ID )
import           Data.Morpheus.Types.Internal.AST
                                                ( Schema(..)
                                                , QUERY
                                                , DataType
                                                , allDataTypes
                                                , lookupDataType
                                                , FieldsDefinition
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Resolver
                                                , resolveUpdates
                                                )


convertTypes
  :: Monad m => Schema -> Resolver QUERY e m [S__Type (Resolver QUERY e m)]
convertTypes lib = traverse (`render` lib) (allDataTypes lib)

buildSchemaLinkType
  :: Monad m => DataType -> S__Type (Resolver QUERY e m)
buildSchemaLinkType DataType { typeName } = createObjectType typeName Nothing $ Just []

findType
  :: Monad m
  => Text
  -> Schema
  -> Resolver QUERY e m (Maybe (S__Type (Resolver QUERY e m)))
findType name lib = renderT (lookupDataType name lib)
 where
  renderT (Just datatype) = Just <$> render (name, datatype) lib
  renderT Nothing         = pure Nothing

initSchema
  :: Monad m
  => Schema
  -> Resolver QUERY e m (S__Schema (Resolver QUERY e m))
initSchema lib = pure S__Schema
  { s__SchemaTypes            = convertTypes lib
  , s__SchemaQueryType        = pure $ buildSchemaLinkType $ query lib
  , s__SchemaMutationType     = pure $ buildSchemaLinkType <$> mutation lib
  , s__SchemaSubscriptionType = pure $ buildSchemaLinkType <$> subscription lib
  , s__SchemaDirectives       = pure []
  }

hiddenRootFields :: FieldsDefinition
hiddenRootFields = fst $ introspectObjectFields
  (Proxy :: Proxy (CUSTOM (Root Maybe)))
  ("Root", OutputType, Proxy @(Root Maybe))

defaultTypes :: TypeUpdater
defaultTypes = flip
  resolveUpdates
  [ introspect (Proxy @Bool)
  , introspect (Proxy @Int)
  , introspect (Proxy @Float)
  , introspect (Proxy @Text)
  , introspect (Proxy @ID)
  , introspect (Proxy @(S__Schema Maybe))
  ]

schemaAPI :: Monad m => Schema -> Root (Resolver QUERY e m)
schemaAPI lib = Root { root__type, root__schema = initSchema lib }
  where root__type (Root__typeArgs name) = findType name lib
