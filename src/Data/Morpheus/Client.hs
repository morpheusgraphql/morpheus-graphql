{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Client
  ( gqlCore
  , schemaByDocument
  , defineQuery
  , defineQueryWith
  , Fetch(..)
  ) where

import           Data.ByteString.Lazy                    (ByteString)
import           Data.Morpheus.Execution.Client.Build    (defineQuery, defineQueryWith)
import           Data.Morpheus.Execution.Client.Compile  (compileSyntax)
import           Data.Morpheus.Execution.Client.Fetch    (Fetch (..))
import           Data.Morpheus.Parsing.Document.Parse    (parseFullGQLDocument)
import           Data.Morpheus.Types.Internal.Data       (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Language.Haskell.TH.Quote

gqlCore :: QuasiQuoter
gqlCore =
  QuasiQuoter
    { quoteExp = compileSyntax
    , quotePat = notHandled "Patterns"
    , quoteType = notHandled "Types"
    , quoteDec = notHandled "Declarations"
    }
  where
    notHandled things = error $ things ++ " are not supported by the GraphQL QuasiQuoter"

schemaByDocument :: IO ByteString -> IO (Validation DataTypeLib)
schemaByDocument documentGQL = parseFullGQLDocument <$> documentGQL
