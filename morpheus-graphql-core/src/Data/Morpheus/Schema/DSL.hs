{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Schema.DSL (dsl) where

import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Parsing.Document.TypeSystem (parseSchema)
import Data.Morpheus.Types.Internal.AST (ANY, TypeDefinition)
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Text
  ( Text,
    pack,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote

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
dslExpression doc = case (parseSchema doc :: Eventless [TypeDefinition ANY]) of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|result|]
