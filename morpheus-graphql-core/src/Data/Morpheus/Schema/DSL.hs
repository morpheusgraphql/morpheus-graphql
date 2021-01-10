{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Schema.DSL (dsl) where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Ext.Result
  ( Result (..),
  )
import Data.Morpheus.Parsing.Document.TypeSystem
  ( parseSchema,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Relude hiding (ByteString)

dsl :: QuasiQuoter
dsl =
  QuasiQuoter
    { quoteExp = dslExpression . pack,
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = notHandled "Declarations"
    }
  where
    notHandled things = error $ things <> " are not supported by the GraphQL QuasiQuoter"

dslExpression :: ByteString -> Q Exp
dslExpression doc = case parseSchema doc of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} -> gqlWarnings warnings >> [|result|]
