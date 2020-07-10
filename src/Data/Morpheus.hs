-- | Build GraphQL APIs with your favourite functional language!
module Data.Morpheus
  ( Interpreter (..),
    compileTimeSchema,
  )
where

import Data.Morpheus.Server.Deriving.Interpreter
  ( Interpreter (..),
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( compileTimeSchema,
  )
