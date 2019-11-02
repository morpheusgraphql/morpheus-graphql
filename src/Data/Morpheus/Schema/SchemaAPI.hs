{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Morpheus.Schema.SchemaAPI
  ( hiddenRootFields
  , defaultTypes
  , schemaAPI
  ) where

import           Data.Proxy
import           Data.Text                                     (Text)

-- MORPHEUS
import           Data.Morpheus.Execution.Internal.GraphScanner (resolveUpdates)
import           Data.Morpheus.Execution.Server.Introspect     (ObjectFields (..), TypeUpdater, introspect)
import           Data.Morpheus.Rendering.RenderIntrospection   (createObjectType, render)
import           Data.Morpheus.Schema.Schema                   (Root (..), Root__typeArgs (..), S__Schema (..), S__Type)
import           Data.Morpheus.Types                           (constRes)
import           Data.Morpheus.Types.GQLType                   (CUSTOM)
import           Data.Morpheus.Types.ID                        (ID)
import           Data.Morpheus.Types.Internal.Data             (DataField (..), DataObject, DataTypeLib (..), QUERY,
                                                                allDataTypes, lookupDataType)
import           Data.Morpheus.Types.Internal.Resolver         (Resolver (..))

convertTypes :: Monad m => DataTypeLib -> Resolver QUERY m e [S__Type (Resolver QUERY m e )]
convertTypes lib = traverse (`render` lib) (allDataTypes lib)

buildSchemaLinkType :: Monad m => (Text, DataObject) -> S__Type (Resolver QUERY m e )
buildSchemaLinkType (key', _) = createObjectType key' Nothing $ Just []

findType :: Monad m => Text -> DataTypeLib -> Resolver QUERY m e (Maybe (S__Type (Resolver QUERY m e )))
findType name lib = renderT (lookupDataType name lib)
  where
    renderT (Just datatype) = Just <$> render (name,datatype) lib
    renderT Nothing         = pure Nothing

initSchema :: Monad m => DataTypeLib -> Resolver QUERY m e (S__Schema (Resolver QUERY m e ))
initSchema lib =
  pure
    S__Schema
      { s__SchemaTypes = const $ convertTypes lib
      , s__SchemaQueryType = constRes $ buildSchemaLinkType $ query lib
      , s__SchemaMutationType = constRes $ buildSchemaLinkType <$> mutation lib
      , s__SchemaSubscriptionType = constRes $ buildSchemaLinkType <$> subscription lib
      , s__SchemaDirectives = constRes []
      }

hideFields :: (Text, DataField) -> (Text, DataField)
hideFields (key', field) = (key', field {fieldHidden = True})

hiddenRootFields :: [(Text, DataField)]
hiddenRootFields = map hideFields $ fst $ objectFields (Proxy :: Proxy (CUSTOM (Root Maybe))) (Proxy @(Root Maybe))

defaultTypes :: TypeUpdater
defaultTypes =
  flip
    resolveUpdates
    [ introspect (Proxy @Bool)
    , introspect (Proxy @Int)
    , introspect (Proxy @Float)
    , introspect (Proxy @Text)
    , introspect (Proxy @ID)
    , introspect (Proxy @(S__Schema Maybe))
    ]

schemaAPI :: Monad m => DataTypeLib -> Root (Resolver QUERY m e)
schemaAPI lib = Root {root__type, root__schema}
  where
    root__type (Root__typeArgs name) = findType name lib
    root__schema _ = initSchema lib
