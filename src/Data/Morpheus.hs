{-# LANGUAGE FlexibleContexts #-}

-- | Build GraphQL APIs with your favourite functional language!
module Data.Morpheus
  ( interpreter,
    debugInterpreter,
    App,
    deriveApp,
    runApp,
    debugApp,
  )
where

-- MORPHEUS
import Data.Morpheus.Core
  ( App,
    debugApp,
    runApp,
  )
import Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
    deriveApp,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Data.Morpheus.Types.IO (MapAPI)

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
debugInterpreter = debugApp . deriveApp
