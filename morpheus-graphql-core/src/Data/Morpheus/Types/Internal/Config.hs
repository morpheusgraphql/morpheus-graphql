{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
    defaultConfig,
    debugConfig,
    SchemaVisitors (..),
  )
where

import Data.Morpheus.Ext.Result (GQLResult)
import Data.Morpheus.Types.Internal.AST.Fields
  ( Directives,
    FieldDefinition,
  )
import Data.Morpheus.Types.Internal.AST.Stage (VALID)
import Data.Morpheus.Types.Internal.AST.TypeCategory (ANY)
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( TypeDefinition,
  )
import Relude
import Prelude (show)

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

data SchemaVisitors = SchemaVisitors
  { typeVisitors :: Directives VALID -> TypeDefinition ANY VALID -> GQLResult (TypeDefinition ANY VALID),
    fieldVisitors :: Directives VALID -> FieldDefinition ANY VALID -> GQLResult (FieldDefinition ANY VALID)
  }

instance Show SchemaVisitors where
  show _ = "Visitors{}"

data Config = Config
  { debug :: Bool,
    validationMode :: VALIDATION_MODE,
    schemaVisitors :: SchemaVisitors
  }
  deriving (Show)

defaultSchemaVisitors :: SchemaVisitors
defaultSchemaVisitors =
  SchemaVisitors
    { fieldVisitors = const pure,
      typeVisitors = const pure
    }

defaultConfig :: Config
defaultConfig =
  Config
    { debug = False,
      validationMode = FULL_VALIDATION,
      schemaVisitors = defaultSchemaVisitors
    }

debugConfig :: Config
debugConfig = defaultConfig {debug = True}
