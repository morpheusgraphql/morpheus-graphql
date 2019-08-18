{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Client
  ( parseGQLWith
  , schemaByDocument
  , defineQuery
  , Fetch(..)
  ) where

import           Data.ByteString.Lazy                    (ByteString)
import           Data.Morpheus.Execution.Client.Build    (defineQuery)
import           Data.Morpheus.Execution.Client.Compile  (compileWith)
import           Data.Morpheus.Execution.Client.Fetch    (Fetch (..))
import           Data.Morpheus.Parsing.Document.Parse    (parseFullGQLDocument)
import           Data.Morpheus.Types.Internal.Data       (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Language.Haskell.TH.Quote

parseGQLWith :: IO (Validation DataTypeLib) -> QuasiQuoter
parseGQLWith schema =
  QuasiQuoter
    { quoteExp = compileWith schema
    , quotePat = notHandled "Patterns"
    , quoteType = notHandled "Types"
    , quoteDec = notHandled "Declarations"
    }
  where
    notHandled things =
      error $ things ++ " are not supported by the GraphQL QuasiQuoter"

schemaByDocument :: IO ByteString -> IO (Validation DataTypeLib)
schemaByDocument documentGQL = parseFullGQLDocument <$> documentGQL
