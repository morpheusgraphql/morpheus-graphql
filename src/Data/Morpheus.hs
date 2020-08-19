-- | Build GraphQL APIs with your favourite functional language!
module Data.Morpheus
  ( Interpreter (..),
    deriveApp,
    runApp,
    debugApp,
  )
where

import Data.Morpheus.Core
  ( debugApp,
    runApp,
  )
import Data.Morpheus.Server.Deriving.Interpreter
  ( Interpreter (..),
  )
import Data.Morpheus.Server.Deriving.Resolve
  ( deriveApp,
  )
