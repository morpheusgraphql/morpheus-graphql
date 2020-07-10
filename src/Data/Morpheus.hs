-- | Build GraphQL APIs with your favourite functional language!
module Data.Morpheus
  ( Interpreter (..),
    compileTimeSchemaValidation,
  )
where

import Data.Morpheus.Server.Deriving.Interpreter
  ( Interpreter (..),
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( compileTimeSchemaValidation,
  )
