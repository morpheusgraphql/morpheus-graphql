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
import Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
    deriveSchema,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDecContext (..),
  )
import Data.Morpheus.Server.TH.Compile
  ( compileDocument,
    gqlDocument,
  )
import Data.Morpheus.Types (RootResolver)
import Data.Morpheus.Types.Internal.Resolving
  ( resultOr,
  )
import Language.Haskell.TH

importGQLDocument :: FilePath -> Q [Dec]
importGQLDocument src =
  runIO (readFile src)
    >>= compileDocument
      ServerDecContext
        { namespace = False
        }

importGQLDocumentWithNamespace :: FilePath -> Q [Dec]
importGQLDocumentWithNamespace src =
  runIO (readFile src)
    >>= compileDocument
      ServerDecContext
        { namespace = True
        }

-- | Generates schema.gql file from 'RootResolver'
toGraphQLDocument ::
  RootResolverConstraint m event query mut sub =>
  proxy (RootResolver m event query mut sub) ->
  ByteString
toGraphQLDocument =
  resultOr (pack . show) render
    . deriveSchema
