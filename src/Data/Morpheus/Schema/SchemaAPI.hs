{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Morpheus.Schema.SchemaAPI
  ( hiddenRootFields
  , defaultTypes
  , schemaAPI
  ) where

import           Data.Proxy
import           Data.Text                                 (Text)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (ObjectRep (..), TypeUpdater, introspectOutputType,
                                                            resolveTypes)
import           Data.Morpheus.Schema.Schema               (Schema, Type, findType, initSchema)
import           Data.Morpheus.Types.ID                    (ID)
import           Data.Morpheus.Types.Internal.Data         (DataField (..), DataOutputField, DataTypeLib (..))

newtype TypeArgs = TypeArgs
  { name :: Text
  } deriving (Generic)

data SchemaAPI = SchemaAPI
  { __type   :: TypeArgs -> Either String (Maybe Type)
  , __schema :: Schema
  } deriving (Generic)

hideFields :: (Text, DataField) -> (Text, DataField)
hideFields (key', field) = (key', field {fieldHidden = True})

hiddenRootFields :: [(Text, DataOutputField)]
hiddenRootFields = map (hideFields . fst) $ objectFieldTypes $ Proxy @(Rep SchemaAPI)

defaultTypes :: TypeUpdater
defaultTypes =
  flip
    resolveTypes
    [ introspectOutputType (Proxy @Bool)
    , introspectOutputType (Proxy @Int)
    , introspectOutputType (Proxy @Float)
    , introspectOutputType (Proxy @Text)
    , introspectOutputType (Proxy @ID)
    , introspectOutputType (Proxy @Schema)
    ]

schemaAPI :: DataTypeLib -> SchemaAPI
schemaAPI lib = SchemaAPI {__type = \TypeArgs {name} -> return $ findType name lib, __schema = initSchema lib}
