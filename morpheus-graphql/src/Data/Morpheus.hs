{-# LANGUAGE NoImplicitPrelude #-}

-- | Build GraphQL APIs with your favorite functional language!
module Data.Morpheus
  ( interpreter,
    debugInterpreter,
    App,
    deriveApp,
    runApp,
    withDebugger,
  )
where

import Data.Morpheus.Server
  ( App,
    debugInterpreter,
    deriveApp,
    interpreter,
    runApp,
    withDebugger,
  )
