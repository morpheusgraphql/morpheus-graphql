module Data.Morpheus.Parsing.Parser (parseTypeSystemDefinition, parseGQL, decodeIntrospection) where

import Control.Monad ((>=>))
import Data.Morpheus.Parsing.Document.TypeSystem (parseSchema)
import Data.Morpheus.Parsing.JSONSchema.Parse
  ( decodeIntrospection,
  )
import Data.Morpheus.Parsing.Request.Parser (parseGQL)
import Data.Morpheus.Types.Internal.AST
  ( TypeDefinition (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
  )
import Data.Text (Text)

parseTypeSystemDefinition ::
  Text -> Eventless [TypeDefinition]
parseTypeSystemDefinition =
  parseSchema >=> validatePartialDocument
