{-# LANGUAGE FlexibleContexts #-}

-- | Build GraphQL APIs with your favourite functional language!
module Data.Morpheus
  ( interpreter,
    debugInterpreter,
    ApiRunner (..),
    deriveApi,
    Api,
  )
where

-- MORPHEUS
import Data.Morpheus.Core
  ( Api,
    ApiRunner (..),
  )
import Data.Morpheus.Server.Deriving.Api
  ( RootResolverConstraint,
    deriveApi,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )

-- | main query processor and resolver
interpreter ::
  ApiRunner e m a b =>
  (RootResolverConstraint m e query mut sub) =>
  RootResolver m e query mut sub ->
  a ->
  b
interpreter = runApi . deriveApi

debugInterpreter ::
  ApiRunner e m a b =>
  (RootResolverConstraint m e query mut sub) =>
  RootResolver m e query mut sub ->
  a ->
  b
debugInterpreter = debugApi . deriveApi
