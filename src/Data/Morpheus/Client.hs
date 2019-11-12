{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Client
  ( gql
  , Fetch(..)
  , defineQuery
  , defineByDocument
  , defineByDocumentFile
  , defineByIntrospection
  , defineByIntrospectionFile
  )
where

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as L
                                                ( readFile )
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Data.Morpheus.Document         ( parseFullGQLDocument )
-- MORPHEUS
import           Data.Morpheus.Execution.Client.Build
                                                ( defineQuery )
import           Data.Morpheus.Execution.Client.Compile
                                                ( compileSyntax )
import           Data.Morpheus.Execution.Client.Fetch
                                                ( Fetch(..) )
import           Data.Morpheus.Parsing.JSONSchema.Parse
                                                ( decodeIntrospection )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataTypeLib )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation )
import           Data.Morpheus.Types.Types      ( GQLQueryRoot )

gql :: QuasiQuoter
gql = QuasiQuoter { quoteExp  = compileSyntax
                  , quotePat  = notHandled "Patterns"
                  , quoteType = notHandled "Types"
                  , quoteDec  = notHandled "Declarations"
                  }
 where
  notHandled things =
    error $ things ++ " are not supported by the GraphQL QuasiQuoter"

defineByDocumentFile :: String -> (GQLQueryRoot, String) -> Q [Dec]
defineByDocumentFile = defineByDocument . L.readFile

defineByIntrospectionFile :: String -> (GQLQueryRoot, String) -> Q [Dec]
defineByIntrospectionFile = defineByIntrospection . L.readFile

--
--
-- TODO: Define By API
-- Validates By Server API
--
defineByDocument :: IO ByteString -> (GQLQueryRoot, String) -> Q [Dec]
defineByDocument doc = defineQuery (schemaByDocument doc)

schemaByDocument :: IO ByteString -> IO (Validation DataTypeLib)
schemaByDocument documentGQL = parseFullGQLDocument <$> documentGQL

defineByIntrospection :: IO ByteString -> (GQLQueryRoot, String) -> Q [Dec]
defineByIntrospection json = defineQuery (decodeIntrospection <$> json)
