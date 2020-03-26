{-#LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Execution.Document.Compile
  ( compileDocument
  , gqlDocument
  , gqlDocumentNamespace
  )
where

import qualified Data.Text                     as T
                                                ( pack )
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

--
--  Morpheus
import           Data.Morpheus.Error.Client.Client
                                                ( renderGQLErrors
                                                , gqlWarnings
                                                )
import           Data.Morpheus.Execution.Document.Convert
                                                ( toTHDefinitions )
import           Data.Morpheus.Execution.Document.Declare
                                                ( declareTypes )
import           Data.Morpheus.Parsing.Document.TypeSystem
                                                ( parseSchema )
import           Data.Morpheus.Validation.Document.Validation
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Result(..) )


gqlDocumentNamespace :: QuasiQuoter
gqlDocumentNamespace = QuasiQuoter { quoteExp  = notHandled "Expressions"
                                   , quotePat  = notHandled "Patterns"
                                   , quoteType = notHandled "Types"
                                   , quoteDec  = compileDocument True
                                   }
 where
  notHandled things =
    error $ things ++ " are not supported by the GraphQL QuasiQuoter"

gqlDocument :: QuasiQuoter
gqlDocument = QuasiQuoter { quoteExp  = notHandled "Expressions"
                          , quotePat  = notHandled "Patterns"
                          , quoteType = notHandled "Types"
                          , quoteDec  = compileDocument False
                          }
 where
  notHandled things =
    error $ things ++ " are not supported by the GraphQL QuasiQuoter"

compileDocument :: Bool -> String -> Q [Dec]
compileDocument namespace documentTXT =
  case
      parseSchema (T.pack documentTXT)
      >>= validatePartialDocument
    of
      Failure errors -> fail (renderGQLErrors errors)
      Success { result = schema, warnings } ->
        gqlWarnings warnings >> toTHDefinitions namespace schema >>= declareTypes namespace
