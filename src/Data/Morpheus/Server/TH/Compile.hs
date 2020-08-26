{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Server.TH.Compile
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
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDecContext (..),
  )
import Data.Morpheus.Server.TH.Declare
  ( declare,
  )
import Data.Morpheus.Server.TH.Transform
  ( toTHDefinitions,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Result (..),
  )
import qualified Data.Text as T
  ( pack,
  )
import Language.Haskell.TH (Dec, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

gqlDocumentNamespace :: QuasiQuoter
gqlDocumentNamespace =
  QuasiQuoter
    { quoteExp = notHandled "Expressions",
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = compileDocument ServerDecContext {namespace = True}
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
      quoteDec = compileDocument ServerDecContext {namespace = False}
    }
  where
    notHandled things =
      error $ things ++ " are not supported by the GraphQL QuasiQuoter"

compileDocument :: ServerDecContext -> String -> Q [Dec]
compileDocument ctx documentTXT =
  case parseTypeDefinitions (T.pack documentTXT) of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result = schema, warnings} ->
      gqlWarnings warnings >> toTHDefinitions (namespace ctx) schema >>= declare ctx
