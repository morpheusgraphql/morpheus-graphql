{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Execution.Client
  ( gql
  , defineQuery
  , Fetch(..)
  ) where

import           Data.ByteString.Lazy                    (ByteString)
import qualified Data.ByteString.Lazy                    as L (readFile)
import           Data.Morpheus.Execution.Client.Build    (defineQuery)
import           Data.Morpheus.Execution.Client.Compile  (compileWith)
import           Data.Morpheus.Execution.Client.Fetch    (Fetch (..))
import           Data.Morpheus.Parsing.Document.Parse    (parseFullGQLDocument)
import           Data.Morpheus.Types.Internal.Data       (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Language.Haskell.TH.Quote

gql :: QuasiQuoter
gql = parseGQLWith $ schemaByDocument (L.readFile "./assets/simple.gql")

parseGQLWith :: IO (Validation DataTypeLib) -> QuasiQuoter
parseGQLWith schema =
  QuasiQuoter
    { quoteExp = compileWith schema
    , quotePat = notHandled "Patterns"
    , quoteType = notHandled "Types"
    , quoteDec = notHandled "Declarations"
    }
  where
    notHandled things = error $ things ++ " are not supported by the GraphQL QuasiQuoter"

schemaByDocument :: IO ByteString -> IO (Validation DataTypeLib)
schemaByDocument documentGQL = parseFullGQLDocument <$> documentGQL
