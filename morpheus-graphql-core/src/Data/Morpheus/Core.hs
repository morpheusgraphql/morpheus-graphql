{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Core
  ( parseFullSchemaDocument,
    parseSchemaDocument,
    parseTypeDefinitions,
    validateRequest,
    parseRequestWith,
    parseRequest,
    SelectionTree (..),
    Config (..),
    VALIDATION_MODE (..),
    defaultConfig,
    debugConfig,
    RenderGQL (..),
    render,
    ValidateSchema (..),
    internalSchema,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Ext.Result
  ( Eventless,
  )
import Data.Morpheus.Ext.SemigroupM
  ( (<:>),
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseRequestWith,
    parseSchemaDocument,
    parseTypeDefinitions,
  )
import Data.Morpheus.Rendering.RenderGQL (RenderGQL (..), render)
import Data.Morpheus.Schema.Schema (internalSchema)
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
import Data.Morpheus.Types.SelectionTree (SelectionTree (..))
import Data.Morpheus.Validation.Document.Validation (ValidateSchema (..))
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import Relude hiding (ByteString)

parseFullSchemaDocument :: ByteString -> Eventless (Schema VALID)
parseFullSchemaDocument = parseSchemaDocument >=> (internalSchema <:>)
