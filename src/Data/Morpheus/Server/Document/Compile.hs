{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Server.Document.Compile
  ( compileDocument,
    gqlDocument,
    gqlDocumentNamespace,
  )
where

--
--  Morpheus

import Data.Morpheus.Core
  ( parseTypeDefinitions,
  )
import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Server.Document.Convert
  ( toTHDefinitions,
  )
import Data.Morpheus.Server.Document.Declare
  ( declareTypes,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Result (..),
  )
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
  case parseTypeDefinitions (T.pack documentTXT) of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result = schema, warnings} ->
      gqlWarnings warnings >> toTHDefinitions namespace schema >>= declareTypes namespace
