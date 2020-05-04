{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Morpheus.Client.Compile
  ( compileSyntax,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Document (parseFullGQLDocument)
import Data.Morpheus.Parsing.Internal
  ( decodeIntrospection,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery,
    Schema,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote

gql :: QuasiQuoter
gql =
  QuasiQuoter
    { quoteExp = compileSyntax,
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = notHandled "Declarations"
    }
  where
    notHandled things =
      error $ things ++ " are not supported by the GraphQL QuasiQuoter"

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

defineByIntrospection :: IO ByteString -> (GQLQuery, String) -> Q [Dec]
defineByIntrospection json = defineQuery (decodeIntrospection <$> json)
