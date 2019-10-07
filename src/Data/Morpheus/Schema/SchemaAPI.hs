{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}
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
import           Data.Morpheus.Types.Internal.Data             (DataField (..), DataObject, DataTypeLib (..),
                                                                allDataTypes)
import           Data.Morpheus.Types.Resolver                  (GQLFail (..), ResolveT, Resolver)

convertTypes :: Monad m => DataTypeLib -> (Resolver m) [S__Type (Resolver m)]
convertTypes lib = traverse (`render` lib) (allDataTypes lib)

buildSchemaLinkType :: Monad m => (Text, DataObject) -> S__Type (Resolver m)
buildSchemaLinkType (key', _) = createObjectType key' Nothing $ Just []

findType :: Monad m => Text -> DataTypeLib -> Resolver m (Maybe (S__Type (Resolver m)))
findType name lib = getType >>= renderT
  where
    getType = pure $ (name, ) <$> lookup name (allDataTypes lib)
    ------------------------------------------------------------
    renderT (Just datatype) = toSuccess (const Nothing) Just (render datatype lib)
    renderT Nothing         = pure Nothing

initSchema :: Monad m => DataTypeLib -> (Resolver m) (S__Schema (Resolver m))
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

schemaAPI :: Monad m => DataTypeLib -> ResolveT m (Root (Resolver m))
schemaAPI lib = pure $ Root {root__type, root__schema}
  where
    root__type (Root__typeArgs name) = findType name lib
    root__schema _ = initSchema lib
