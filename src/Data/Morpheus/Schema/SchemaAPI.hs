{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

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
                                                ( objectFields
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
import           Data.Morpheus.Types            ( constRes )
import           Data.Morpheus.Types.GQLType    ( CUSTOM )
import           Data.Morpheus.Types.ID         ( ID )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataField(..)
                                                , DataObject
                                                , DataTypeLib(..)
                                                , QUERY
                                                , DataType
                                                , allDataTypes
                                                , lookupDataType
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Resolver(..)
                                                , resolveUpdates
                                                )


convertTypes
  :: Monad m => DataTypeLib -> Resolver QUERY e m [S__Type (Resolver QUERY e m)]
convertTypes lib = traverse (`render` lib) (allDataTypes lib)

buildSchemaLinkType
  :: Monad m => (Text, DataType) -> S__Type (Resolver QUERY e m)
buildSchemaLinkType (key', _) = createObjectType key' Nothing $ Just []

findType
  :: Monad m
  => Text
  -> DataTypeLib
  -> Resolver QUERY e m (Maybe (S__Type (Resolver QUERY e m)))
findType name lib = renderT (lookupDataType name lib)
 where
  renderT (Just datatype) = Just <$> render (name, datatype) lib
  renderT Nothing         = pure Nothing

initSchema
  :: Monad m
  => DataTypeLib
  -> Resolver QUERY e m (S__Schema (Resolver QUERY e m))
initSchema lib = pure S__Schema
  { s__SchemaTypes            = const $ convertTypes lib
  , s__SchemaQueryType        = constRes $ buildSchemaLinkType $ query lib
  , s__SchemaMutationType     = constRes $ buildSchemaLinkType <$> mutation lib
  , s__SchemaSubscriptionType = constRes
                                $   buildSchemaLinkType
                                <$> subscription lib
  , s__SchemaDirectives       = constRes []
  }

hiddenRootFields :: [(Text, DataField)]
hiddenRootFields = fst $ objectFields (Proxy :: Proxy (CUSTOM (Root Maybe)))
                                      (OutputType, Proxy @(Root Maybe))

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

schemaAPI :: Monad m => DataTypeLib -> Root (Resolver QUERY e m)
schemaAPI lib = Root { root__type, root__schema }
 where
  root__type (Root__typeArgs name) = findType name lib
  root__schema _ = initSchema lib
