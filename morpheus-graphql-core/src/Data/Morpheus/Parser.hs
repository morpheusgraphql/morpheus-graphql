{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parser
  ( parseSchema,
    parseTypeDefinitions,
    parseDefinitions,
    parseRequest,
    parseRequestWith,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Ext.Result
  ( GQLResult,
    sortErrors,
  )
import Data.Morpheus.Internal.Utils ((<:>))
import Data.Morpheus.Parsing.Document.TypeSystem
  ( parseDefinitions,
    parseSchemaWithoutValidation,
    parseTypeDefinitions,
  )
import Data.Morpheus.Parsing.Request.Parser
  ( parseRequest,
  )
import Data.Morpheus.Schema.Schema (internalSchema)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Operation,
    Schema (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
  )
import Data.Morpheus.Validation.Document.Validation
  ( validateSchema,
  )
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import Relude hiding (ByteString)

parseSchema ::
  ByteString -> GQLResult (Schema VALID)
parseSchema =
  sortErrors
    . ( parseSchemaWithoutValidation
          >=> validateSchema
            True
            Config
              { debug = False,
                introspection = True,
                validationMode = FULL_VALIDATION
              }
      )

parseRequestWith :: Config -> Schema VALID -> GQLRequest -> GQLResult (Operation VALID)
parseRequestWith config schema req = do
  qu <- parseRequest req
  fillSchema <- internalSchema <:> schema
  validateRequest config fillSchema qu
