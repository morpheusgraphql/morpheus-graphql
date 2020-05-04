{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.QuasiQuoter
  ( gql,
    dsl,
    compileSyntax,
  )
where

import Control.Monad ((>=>))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( pack,
    readFile,
  )
-- MORPHEUS

import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import Data.Morpheus.Client.Selection
  ( operationTypes,
  )
import Data.Morpheus.Core
  ( decodeIntrospection,
    parseGraphQLDocument,
    parseTypeDefinitions,
  )
import Data.Morpheus.Error.Warning
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Parsing.Internal
  ( parseRequest,
  )
import Data.Morpheus.Types.IO (GQLRequest (..))
--
import qualified Data.Morpheus.Types.Internal.AST as O
  ( Operation (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ClientQuery (..),
    GQLQuery (..),
    Schema,
    VALIDATION_MODE (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import qualified Data.Text as T
  ( pack,
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

dsl :: QuasiQuoter
dsl =
  QuasiQuoter
    { quoteExp = compileDocument,
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = notHandled "Declarations"
    }
  where
    notHandled things =
      error $ things ++ " are not supported by the GraphQL QuasiQuoter"

compileSyntax :: String -> Q Exp
compileSyntax queryText = case parseRequest request of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|(result, queryText)|]
  where
    request =
      GQLRequest
        { query = T.pack queryText,
          operationName = Nothing,
          variables = Nothing
        }

compileDocument :: String -> Q Exp
compileDocument doc = case parseTypeDefinitions (T.pack doc) of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|result|]
