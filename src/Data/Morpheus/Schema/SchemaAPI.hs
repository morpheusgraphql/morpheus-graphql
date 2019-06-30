{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Morpheus.Schema.SchemaAPI
  ( hiddenRootFields
  , schemaTypes
  , schemaAPI
  ) where

import           Data.Proxy
import           Data.Text                         (Text)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Resolve.Introspect  (ObjectRep (..), TypeUpdater, introspectOutputType)
import           Data.Morpheus.Schema.Schema       (Schema, Type, findType, initSchema)
import           Data.Morpheus.Types.Internal.Data (DataField (..), DataOutputField, DataTypeLib (..))

newtype TypeArgs = TypeArgs
  { name :: Text
  } deriving (Generic)

data SchemaAPI = SchemaAPI
  { __type   :: TypeArgs -> Either String (Maybe Type)
  , __schema :: Schema
  } deriving (Generic)

hideFields :: (Text, DataField a) -> (Text, DataField a)
hideFields (key', field) = (key', field {fieldHidden = True})

hiddenRootFields :: [(Text, DataOutputField)]
hiddenRootFields = map (hideFields . fst) $ objectFieldTypes $ Proxy @(Rep SchemaAPI)

schemaTypes :: TypeUpdater
schemaTypes = introspectOutputType (Proxy @Schema)

schemaAPI :: DataTypeLib -> SchemaAPI
schemaAPI lib = SchemaAPI {__type = \TypeArgs {name} -> return $ findType name lib, __schema = initSchema lib}
