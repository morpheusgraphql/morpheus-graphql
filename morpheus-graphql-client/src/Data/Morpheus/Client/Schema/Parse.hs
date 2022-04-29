module Data.Morpheus.Client.Schema.Parse
  ( parseSchema,
  )
where

import Data.Morpheus.Client.Internal.Types (Source (..))
import Data.Morpheus.Client.Schema.JSON.Parse
  ( decodeIntrospection,
  )
import Data.Morpheus.Core
  ( parseFullSchema,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST (Schema, VALID)

parseSchema :: Source -> GQLResult (Schema VALID)
parseSchema (JSON doc) = decodeIntrospection doc
parseSchema (GQL doc) = parseFullSchema doc
