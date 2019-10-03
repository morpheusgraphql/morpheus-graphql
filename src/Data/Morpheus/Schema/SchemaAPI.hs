{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
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
import           Data.Text                                   (Text)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect   (ObjectFields (..), TypeUpdater, introspect, resolveTypes)
import           Data.Morpheus.Rendering.RenderIntrospection (createObjectType, render)
import           Data.Morpheus.Schema.Schema                 (S__Schema (..), S__Type)
import           Data.Morpheus.Types.GQLType                 (FALSE, GQLType (..))
import           Data.Morpheus.Types.ID                      (ID)
import           Data.Morpheus.Types.Internal.Data           (DataField (..), DataObject, DataTypeLib (..),
                                                              allDataTypes)

convertTypes :: Monad m => DataTypeLib -> Either String [S__Type m]
convertTypes lib = traverse (`render` lib) (allDataTypes lib)

buildSchemaLinkType :: Monad m => (Text, DataObject) -> S__Type m
buildSchemaLinkType (key', _) = createObjectType key' Nothing $ Just []

findType :: Monad m => Text -> DataTypeLib -> Maybe (S__Type m)
findType name lib = (name, ) <$> lookup name (allDataTypes lib) >>= renderT
  where
    renderT i =
      case render i lib of
        Left _  -> Nothing
        Right x -> Just x

initSchema :: DataTypeLib -> Either String (S__Schema (Either String))
initSchema lib =
  pure
    S__Schema
      { s__SchemaTypes = const $ convertTypes lib
      , s__SchemaQueryType = const $ pure $ buildSchemaLinkType $ query lib
      , s__SchemaMutationType = const $ pure $ buildSchemaLinkType <$> mutation lib
      , s__SchemaSubscriptionType = const $ pure $ buildSchemaLinkType <$> subscription lib
      , s__SchemaDirectives = const $ return []
      }

newtype TypeArgs = TypeArgs
  { name :: Text
  } deriving (Generic)

data SchemaAPI = SchemaAPI
  { __type   :: TypeArgs -> Either String (Maybe (S__Type (Either String)))
  , __schema :: () -> Either String (S__Schema (Either String))
  } deriving (Generic, GQLType)

hideFields :: (Text, DataField) -> (Text, DataField)
hideFields (key', field) = (key', field {fieldHidden = True})

hiddenRootFields :: [(Text, DataField)]
hiddenRootFields = map hideFields $ fst $ objectFields (Proxy :: Proxy FALSE) (Proxy @SchemaAPI)

defaultTypes :: TypeUpdater
defaultTypes =
  flip
    resolveTypes
    [ introspect (Proxy @Bool)
    , introspect (Proxy @Int)
    , introspect (Proxy @Float)
    , introspect (Proxy @Text)
    , introspect (Proxy @ID)
    , introspect (Proxy @(S__Schema Maybe))
    ]

schemaAPI :: DataTypeLib -> SchemaAPI
schemaAPI lib = SchemaAPI {__type, __schema}
  where
    __type TypeArgs {name} = return $ findType name lib
    __schema _ = initSchema lib
