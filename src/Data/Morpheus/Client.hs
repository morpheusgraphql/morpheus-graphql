{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Client
  ( gql
  , schemaByDocument
  , defineQuery
  , defineByDocument
  , Fetch(..)
  ) where

import           Data.ByteString.Lazy                    (ByteString)
import           Data.Morpheus.Execution.Client.Build    (defineQuery)
import           Data.Morpheus.Execution.Client.Compile  (compileSyntax)
import           Data.Morpheus.Execution.Client.Fetch    (Fetch (..))
import           Data.Morpheus.Parsing.Document.Parse    (parseFullGQLDocument)
import           Data.Morpheus.Types.Internal.Data       (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.Types               (GQLQueryRoot)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

gql :: QuasiQuoter
gql =
  QuasiQuoter
    { quoteExp = compileSyntax
    , quotePat = notHandled "Patterns"
    , quoteType = notHandled "Types"
    , quoteDec = notHandled "Declarations"
    }
  where
    notHandled things = error $ things ++ " are not supported by the GraphQL QuasiQuoter"

defineByDocument :: IO ByteString -> (GQLQueryRoot, String) -> Q [Dec]
defineByDocument doc = defineQuery (schemaByDocument doc)

schemaByDocument :: IO ByteString -> IO (Validation DataTypeLib)
schemaByDocument documentGQL = parseFullGQLDocument <$> documentGQL
