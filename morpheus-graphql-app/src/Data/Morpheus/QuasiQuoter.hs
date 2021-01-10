{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.QuasiQuoter
  ( gql,
    gqlExpression,
    dsl,
    dslExpression,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
    unpack,
  )
import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Internal.Utils
  ( fromLBS,
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseTypeSystemDefinition,
  )
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.Resolving
  ( Result (..),
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Relude hiding (ByteString)

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
      error $
        things
          <> " are not supported by the GraphQL QuasiQuoter"

gqlExpression :: ByteString -> Q Exp
gqlExpression queryText = case parseRequest request of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|(result, query)|]
  where
    query = unpack queryText
    request =
      GQLRequest
        { query = fromLBS queryText,
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
      error $
        things <> " are not supported by the GraphQL QuasiQuoter"

dslExpression :: ByteString -> Q Exp
dslExpression doc = case parseTypeSystemDefinition doc of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|result|]
