module Data.Morpheus.Parsing.Parser (parseTypeDefinitions, parseTypeSystemDefinition, parseRequestWith, parseRequest, decodeIntrospection) where

import Control.Monad ((>=>))
import Data.Morpheus.Parsing.Document.TypeSystem (parseSchema)
import Data.Morpheus.Parsing.JSONSchema.Parse
  ( decodeIntrospection,
  )
import Data.Morpheus.Parsing.Request.Parser (parseGQL)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery (..),
    Operation,
    Schema (..),
    TypeDefinition (..),
    VALID,
    VALIDATION_MODE (..),
    createDataTypeLib,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
  )
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import Data.Text (Text)

parseTypeSystemDefinition ::
  Text -> Eventless Schema
parseTypeSystemDefinition =
  parseSchema >=> createDataTypeLib

parseTypeDefinitions ::
  Text -> Eventless [TypeDefinition]
parseTypeDefinitions =
  parseSchema >=> validatePartialDocument

parseRequest :: GQLRequest -> Eventless GQLQuery
parseRequest = parseGQL

parseRequestWith :: Schema -> GQLRequest -> Eventless (Operation VALID)
parseRequestWith schema = parseRequest >=> validateRequest schema FULL_VALIDATION
