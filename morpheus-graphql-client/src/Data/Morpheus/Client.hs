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
import Data.Morpheus.Core
  ( decodeIntrospection,
    parseFullGQLDocument,
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

defineByDocument :: IO ByteString -> (GQLQuery, String) -> Q [Dec]
defineByDocument doc = defineQuery (schemaByDocument doc)

schemaByDocument :: IO ByteString -> IO (Eventless Schema)
schemaByDocument documentGQL = parseFullGQLDocument <$> documentGQL

defineByIntrospection :: IO ByteString -> (GQLQuery, String) -> Q [Dec]
defineByIntrospection json = defineQuery (decodeIntrospection <$> json)
