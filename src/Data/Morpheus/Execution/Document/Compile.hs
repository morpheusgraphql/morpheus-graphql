{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Execution.Document.Compile
  ( compileDocument,
    gqlDocument,
    gqlDocumentNamespace,
  )
where

--
--  Morpheus
import Data.Morpheus.Error.Client.Client
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Execution.Document.Convert
  ( toTHDefinitions,
  )
import Data.Morpheus.Execution.Document.Declare
  ( declareTypes,
  )
import Data.Morpheus.Parsing.Parser
  ( parseSchema,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Result (..),
  )
import Data.Morpheus.Validation.Document.Validation
import qualified Data.Text as T
  ( pack,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote

gqlDocumentNamespace :: QuasiQuoter
gqlDocumentNamespace =
  QuasiQuoter
    { quoteExp = notHandled "Expressions",
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = compileDocument True
    }
  where
    notHandled things =
      error $ things ++ " are not supported by the GraphQL QuasiQuoter"

gqlDocument :: QuasiQuoter
gqlDocument =
  QuasiQuoter
    { quoteExp = notHandled "Expressions",
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = compileDocument False
    }
  where
    notHandled things =
      error $ things ++ " are not supported by the GraphQL QuasiQuoter"

compileDocument :: Bool -> String -> Q [Dec]
compileDocument namespace documentTXT =
  case parseSchema (T.pack documentTXT)
    >>= validatePartialDocument of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result = schema, warnings} ->
      gqlWarnings warnings >> toTHDefinitions namespace schema >>= declareTypes namespace
