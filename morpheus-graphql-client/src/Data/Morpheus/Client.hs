{-# LANGUAGE FlexibleContexts #-}

module Data.Morpheus.Client
  ( gql,
    Fetch (..),
    defineQuery,
    defineByDocument,
    defineByDocumentFile,
    defineByIntrospection,
    defineByIntrospectionFile,
  )
where

import Control.Monad ((>=>))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
-- MORPHEUS

import Data.Morpheus.Client.Build
  ( defineQuery,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.Schema
  ( defaultTypes,
  )
import Data.Morpheus.Core
  ( decodeIntrospection,
    parseGraphQLDocument,
  )
import Data.Morpheus.QuasiQuoter (gql)
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery,
    Schema,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Language.Haskell.TH

defineByDocumentFile :: String -> (GQLQuery, String) -> Q [Dec]
defineByDocumentFile = defineByDocument . L.readFile

defineByIntrospectionFile :: String -> (GQLQuery, String) -> Q [Dec]
defineByIntrospectionFile = defineByIntrospection . L.readFile

--
--
-- TODO: Define By API
-- Validates By Server API
--
defineByDocument :: IO ByteString -> (GQLQuery, String) -> Q [Dec]
defineByDocument doc = defineQuery (schemaByDocument doc)

schemaByDocument :: IO ByteString -> IO (Eventless Schema)
schemaByDocument documentGQL = parseFullGQLDocument <$> documentGQL

parseFullGQLDocument :: ByteString -> Eventless Schema
parseFullGQLDocument = parseGraphQLDocument >=> defaultTypes

defineByIntrospection :: IO ByteString -> (GQLQuery, String) -> Q [Dec]
defineByIntrospection json = defineQuery (decodeIntrospection <$> json)
