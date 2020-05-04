{-# LANGUAGE FlexibleContexts #-}

module Data.Morpheus.Document
  ( toGraphQLDocument,
    gqlDocument,
    importGQLDocument,
    importGQLDocumentWithNamespace,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
--
-- MORPHEUS
import Data.Morpheus.Rendering.RenderGQL
  ( renderGraphQLDocument,
  )
import Data.Morpheus.Server.Deriving.Resolve
  ( RootResCon,
    fullSchema,
  )
import Data.Morpheus.Server.Document.Compile
  ( compileDocument,
    gqlDocument,
  )
import Data.Morpheus.Types (GQLRootResolver)
import Data.Morpheus.Types.Internal.Resolving
  ( resultOr,
  )
import Language.Haskell.TH

importGQLDocument :: String -> Q [Dec]
importGQLDocument src = runIO (readFile src) >>= compileDocument False

importGQLDocumentWithNamespace :: String -> Q [Dec]
importGQLDocumentWithNamespace src =
  runIO (readFile src) >>= compileDocument True

-- | Generates schema.gql file from 'GQLRootResolver'
toGraphQLDocument ::
  RootResCon m event query mut sub =>
  proxy (GQLRootResolver m event query mut sub) ->
  ByteString
toGraphQLDocument =
  resultOr (pack . show) renderGraphQLDocument
    . fullSchema
