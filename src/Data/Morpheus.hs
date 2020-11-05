{-# LANGUAGE FlexibleContexts #-}
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

-- MORPHEUS
import Data.Morpheus.Core
  ( App,
    runApp,
    withDebugger,
  )
import Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
    deriveApp,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Data.Morpheus.Types.IO (MapAPI)
import Relude

-- | main query processor and resolver
interpreter ::
  (MapAPI a b, RootResolverConstraint m e query mut sub) =>
  RootResolver m e query mut sub ->
  a ->
  m b
interpreter = runApp . deriveApp

debugInterpreter ::
  (MapAPI a b, RootResolverConstraint m e query mut sub) =>
  RootResolver m e query mut sub ->
  a ->
  m b
debugInterpreter = runApp . withDebugger . deriveApp
