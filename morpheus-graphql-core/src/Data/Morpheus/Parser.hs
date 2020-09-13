module Data.Morpheus.Parser
  ( parseTypeDefinitions,
    parseTypeSystemDefinition,
    parseRequest,
    parseRequestWith,
  )
where

import Control.Monad ((>=>))
import Data.Morpheus.Internal.Utils ((<:>))
import qualified Data.Morpheus.Parsing.Document.TypeSystem as P
  ( parseSchema,
    parseTypeDefinitions,
  )
import Data.Morpheus.Parsing.Request.Parser (parseGQL)
import Data.Morpheus.Schema.Schema (internalSchema)
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
  )
import Data.Morpheus.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Validation.Document.Validation
  ( validateSchema,
  )
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import Data.Text (Text)

parseTypeSystemDefinition ::
  Text -> Eventless (Schema VALID)
parseTypeSystemDefinition =
  P.parseSchema
    >=> validateSchema
      True
      Config
        { debug = False,
          validationMode = FULL_VALIDATION
        }

parseTypeDefinitions ::
  Text -> Eventless [TypeDefinition ANY CONST]
parseTypeDefinitions = P.parseTypeDefinitions

parseRequest :: GQLRequest -> Eventless GQLQuery
parseRequest = parseGQL

parseRequestWith :: Config -> Schema VALID -> GQLRequest -> Eventless (Operation VALID)
parseRequestWith config schema req = do
  qu <- parseRequest req
  fuillSchema <- internalSchema <:> schema
  validateRequest config fuillSchema qu
