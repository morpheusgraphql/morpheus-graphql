{-# LANGUAGE NoImplicitPrelude #-}

-- | Build GraphQL APIs with your favorite functional language!
module Data.Morpheus
  ( interpreter,
    debugInterpreter,
    App,
    deriveApp,
    runApp,
    withDebugger,
    httpPlayground,
    compileTimeSchemaValidation,
  )
where

import Data.Morpheus.Server
  ( App,
    compileTimeSchemaValidation,
    debugInterpreter,
    deriveApp,
    httpPlayground,
    interpreter,
    runApp,
    withDebugger,
  )
