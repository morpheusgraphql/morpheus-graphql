module Data.Morpheus.Parser
  ( parseTypeDefinitions,
    parseTypeSystemDefinition,
    parseRequest,
    parseRequestWith,
  )
where

import Control.Monad ((>=>))
import qualified Data.Morpheus.Parsing.Document.TypeSystem as P
  ( parseSchema,
    parseTypeDefinitions,
  )
import Data.Morpheus.Parsing.Request.Parser (parseGQL)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    GQLQuery (..),
    Operation,
    Schema (..),
    TypeDefinition (..),
    VALID,
    VALIDATION_MODE (..),
    buildSchema,
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
  Text -> Eventless (Schema CONST)
parseTypeSystemDefinition =
  P.parseSchema >=> buildSchema

parseTypeDefinitions ::
  Text -> Eventless [TypeDefinition ANY CONST]
parseTypeDefinitions =
  P.parseTypeDefinitions >=> validatePartialDocument

parseRequest :: GQLRequest -> Eventless GQLQuery
parseRequest = parseGQL

parseRequestWith :: Schema VALID -> GQLRequest -> Eventless (Operation VALID)
parseRequestWith schema = parseRequest >=> validateRequest schema FULL_VALIDATION
