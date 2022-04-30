module Data.Morpheus.Client.Schema.Parse
  ( parseSchema,
  )
where

import Data.Morpheus.Client.Internal.Types (SchemaSource (..))
import Data.Morpheus.Client.Schema.JSON.Parse
  ( decodeIntrospection,
  )
import Data.Morpheus.Core
  ( parseFullSchema,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST (Schema, VALID)

parseSchema :: SchemaSource -> GQLResult (Schema VALID)
parseSchema (JSON doc) = decodeIntrospection doc
parseSchema (GQL doc) = parseFullSchema doc
