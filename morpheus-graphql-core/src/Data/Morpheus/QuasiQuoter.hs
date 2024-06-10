{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
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
import Data.Morpheus.Ext.Result
  ( Result (..),
  )
import Data.Morpheus.Internal.Utils
  ( fromLBS,
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseSchema,
  )
import Data.Morpheus.Types.IO (GQLRequest (..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Relude hiding (ByteString)

notSupported :: Text -> a
notSupported things =
  error
    $ things
    <> " are not supported by the GraphQL QuasiQuoter"

gql :: QuasiQuoter
gql =
  QuasiQuoter
    { quoteExp = gqlExpression . pack,
      quotePat = notSupported "Patterns",
      quoteType = notSupported "Types",
      quoteDec = notSupported "Declarations"
    }

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
      quotePat = notSupported "Patterns",
      quoteType = notSupported "Types",
      quoteDec = notSupported "Declarations"
    }

dslExpression :: ByteString -> Q Exp
dslExpression doc = case parseSchema doc of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|result|]
