{-# LANGUAGE FlexibleContexts #-}

-- | Build GraphQL APIs with your favourite functional language!
module Data.Morpheus
  ( interpreter,
    debugInterpreter,
    App,
    AppRunner (..),
    deriveApp,
  )
where

-- MORPHEUS
import Data.Morpheus.Core
  ( App,
    AppRunner (..),
  )
import Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
    deriveApp,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )

-- | main query processor and resolver
interpreter ::
  AppRunner e m a b =>
  (RootResolverConstraint m e query mut sub) =>
  RootResolver m e query mut sub ->
  a ->
  b
interpreter = runApp . deriveApp

debugInterpreter ::
  AppRunner e m a b =>
  (RootResolverConstraint m e query mut sub) =>
  RootResolver m e query mut sub ->
  a ->
  b
debugInterpreter = debugApp . deriveApp
