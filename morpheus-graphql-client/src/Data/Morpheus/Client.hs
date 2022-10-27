{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client
  ( raw,
    Fetch (..),
    FetchError (..),
    ScalarValue (..),
    DecodeScalar (..),
    EncodeScalar (..),
    ID (..),
    declareGlobalTypes,
    declareGlobalTypesByName,
    declareLocalTypes,
    declareLocalTypesInline,
    clientTypeDeclarations,
    -- Fetch API
    GQLClient,
    GQLClientResult,
    ResponseStream,
    withHeaders,
    request,
    forEach,
    single,
    parseClientTypeDeclarations,
    readSchemaSource,
    SchemaSource,
    RequestType (..),
  )
where

import Data.Morpheus.Client.CodeGen.Declare
  ( clientTypeDeclarations,
    declareGlobalTypes,
    declareGlobalTypesByName,
    declareLocalTypes,
    declareLocalTypesInline,
    parseClientTypeDeclarations,
    raw,
  )
import Data.Morpheus.Client.CodeGen.Utils (readSchemaSource)
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.Fetch.RequestType
import Data.Morpheus.Client.Fetch.ResponseStream
  ( GQLClient,
    ResponseStream,
    forEach,
    request,
    single,
    withHeaders,
  )
import Data.Morpheus.Client.Fetch.Types
  ( FetchError (..),
    GQLClientResult,
    SchemaSource (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.ID (ID (..))
import Data.Morpheus.Types.Internal.AST
  ( ScalarValue (..),
  )
