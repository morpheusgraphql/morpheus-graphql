{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App
  ( parseDSL,
    parseFullGQLDocument,
    parseGQLDocument,
    parseTypeSystemDefinition,
    parseTypeDefinitions,
    validateRequest,
    parseRequestWith,
    validateSchema,
    parseRequest,
    SelectionTree (..),
    Config (..),
    VALIDATION_MODE (..),
    defaultConfig,
    debugConfig,
    App (..),
    AppData (..),
    runApp,
    withDebugger,
    mkApp,
    runAppStream,
    render,
    RenderGQL,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Ext.SemigroupM
  ( (<:>),
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseRequestWith,
    parseTypeDefinitions,
    parseTypeSystemDefinition,
  )
import Data.Morpheus.Rendering.RenderGQL (RenderGQL)
import qualified Data.Morpheus.Rendering.RenderGQL as R
import Data.Morpheus.Schema.Schema (internalSchema)
import Data.Morpheus.Types.App
  ( App (..),
    AppData (..),
    mkApp,
    runApp,
    runAppStream,
    withDebugger,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )
import Data.Morpheus.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
    debugConfig,
    defaultConfig,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    resultOr,
    sortErrors,
  )
import Data.Morpheus.Types.SelectionTree (SelectionTree (..))
import Data.Morpheus.Validation.Document.Validation (ValidateSchema (..))
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import Relude hiding (ByteString)

render :: RenderGQL a => a -> ByteString
render = R.renderGQL

parseDSL :: ByteString -> Either String (Schema VALID)
parseDSL = resultOr (Left . show) pure . parseGQLDocument

parseGQLDocument :: ByteString -> Eventless (Schema VALID)
parseGQLDocument = sortErrors . parseTypeSystemDefinition

parseFullGQLDocument :: ByteString -> Eventless (Schema VALID)
parseFullGQLDocument = parseGQLDocument >=> (internalSchema <:>)
