{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( httpPlayground,
    compileTimeSchemaValidation,
    printSchema,
    RootResolverConstraint,
    interpreter,
    debugInterpreter,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import Data.Morpheus.App.Internal.Resolving
  ( resultOr,
  )
import Data.Morpheus.Core
  ( render,
  )
import Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
    deriveSchema,
  )
import Data.Morpheus.Server.Deriving.Schema
  ( compileTimeSchemaValidation,
  )
import Data.Morpheus.Server.Playground
  ( httpPlayground,
  )
import Data.Morpheus.Server.Types (RootResolver)
import Relude hiding (ByteString)

-- | Generates schema.gql file from 'RootResolver'
printSchema ::
  RootResolverConstraint m event query mut sub =>
  proxy (RootResolver m event query mut sub) ->
  ByteString
printSchema =
  resultOr (pack . show) render
    . deriveSchema

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
