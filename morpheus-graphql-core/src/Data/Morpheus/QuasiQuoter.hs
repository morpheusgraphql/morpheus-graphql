{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.QuasiQuoter
  ( gql,
    gqlExpression,
    dsl,
    dslExpression,
  )
where

import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseTypeDefinitions,
  )
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.Resolving
  ( Result (..),
  )
import Data.Text
  ( Text,
    pack,
    unpack,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote

gql :: QuasiQuoter
gql =
  QuasiQuoter
    { quoteExp = gqlExpression . pack,
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = notHandled "Declarations"
    }
  where
    notHandled things =
      error $ things ++ " are not supported by the GraphQL QuasiQuoter"

gqlExpression :: Text -> Q Exp
gqlExpression queryText = case parseRequest request of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|(result, query)|]
  where
    query = unpack queryText
    request =
      GQLRequest
        { query = queryText,
          operationName = Nothing,
          variables = Nothing
        }

dsl :: QuasiQuoter
dsl =
  QuasiQuoter
    { quoteExp = dslExpression . pack,
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = notHandled "Declarations"
    }
  where
    notHandled things =
      error $ things ++ " are not supported by the GraphQL QuasiQuoter"

dslExpression :: Text -> Q Exp
dslExpression doc = case parseTypeDefinitions doc of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|result|]
