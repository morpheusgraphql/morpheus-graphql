{-# LANGUAGE FlexibleContexts #-}

module Data.Morpheus.Document
  ( toGraphQLDocument,
    gqlDocument,
    parseFullGQLDocument,
    importGQLDocument,
    importGQLDocumentWithNamespace,
    parseDSL,
  )
where

import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
--
-- MORPHEUS

import Data.Morpheus.Core
  ( parseDSL,
    parseFullGQLDocument,
  )
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
import Data.Morpheus.Server.Schema.SchemaAPI (defaultTypes)
import Data.Morpheus.Types (GQLRootResolver)
import Data.Morpheus.Types.Internal.AST
  ( Schema,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import qualified Data.Text.Lazy as LT
  ( toStrict,
  )
import Data.Text.Lazy.Encoding (decodeUtf8)
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
toGraphQLDocument x = case fullSchema x of
  Failure errors -> pack (show errors)
  Success {result = lib} -> renderGraphQLDocument lib
