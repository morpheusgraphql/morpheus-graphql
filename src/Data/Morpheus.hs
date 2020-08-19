-- | Build GraphQL APIs with your favourite functional language!
module Data.Morpheus
  ( Interpreter (..),
    AppRunner (..),
    deriveApp,
  )
where

import Data.Morpheus.Server.Deriving.Interpreter
  ( AppRunner (..),
    Interpreter (..),
  )
import Data.Morpheus.Server.Deriving.Resolve
  ( deriveApp,
  )
