{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Core
  ( parseDSL,
    parseFullGQLDocument,
    parseGQLDocument,
    parseTypeSystemDefinition,
    parseTypeDefinitions,
    validateRequest,
    parseRequestWith,
    validateSchema,
    parseRequest,
    RenderGQL (..),
    SelectionTree (..),
    Config (..),
    VALIDATION_MODE (..),
    defaultConfig,
    debugConfig,
    App (..),
    AppData (..),
    runApp,
    debugApp,
    mkApp,
    runAppStream,
  )
where

-- MORPHEUS
import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Internal.Utils
  ( (<:>),
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseRequestWith,
    parseTypeDefinitions,
    parseTypeSystemDefinition,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Schema.Schema (internalSchema)
import Data.Morpheus.Types.App
  ( App (..),
    AppData (..),
    debugApp,
    mkApp,
    runApp,
    runAppStream,
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
  )
import Data.Morpheus.Types.SelectionTree (SelectionTree (..))
import Data.Morpheus.Validation.Document.Validation (ValidateSchema (..))
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import qualified Data.Text.Lazy as LT
  ( toStrict,
  )
import Data.Text.Lazy.Encoding (decodeUtf8)

parseDSL :: ByteString -> Either String (Schema VALID)
parseDSL = resultOr (Left . show) pure . parseGQLDocument

parseGQLDocument :: ByteString -> Eventless (Schema VALID)
parseGQLDocument = parseTypeSystemDefinition . LT.toStrict . decodeUtf8

parseFullGQLDocument :: ByteString -> Eventless (Schema VALID)
parseFullGQLDocument = parseGQLDocument >=> (internalSchema <:>)
