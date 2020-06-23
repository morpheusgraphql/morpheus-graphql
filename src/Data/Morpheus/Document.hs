{-# LANGUAGE FlexibleContexts #-}

module Data.Morpheus.Document
  ( toGraphQLDocument,
    gqlDocument,
    importGQLDocument,
    importGQLDocumentWithNamespace,
    RootResolverConstraint,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import Data.Morpheus.Core
  ( render,
  )
import Data.Morpheus.Server.Deriving.Resolve
  ( RootResolverConstraint,
    fullSchema,
  )
import Data.Morpheus.Server.TH.Compile
  ( compileDocument,
    gqlDocument,
  )
import Data.Morpheus.Types (RootResolver)
import Data.Morpheus.Types.Internal.Resolving
  ( resultOr,
  )
import qualified Data.Text.Lazy as LT
  ( fromStrict,
  )
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Haskell.TH

importGQLDocument :: String -> Q [Dec]
importGQLDocument src = runIO (readFile src) >>= compileDocument False

importGQLDocumentWithNamespace :: String -> Q [Dec]
importGQLDocumentWithNamespace src =
  runIO (readFile src) >>= compileDocument True

-- | Generates schema.gql file from 'RootResolver'
toGraphQLDocument ::
  RootResolverConstraint m event query mut sub =>
  proxy (RootResolver m event query mut sub) ->
  ByteString
toGraphQLDocument =
  resultOr (pack . show) (encodeUtf8 . LT.fromStrict . render)
    . fullSchema
