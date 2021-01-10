{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import Data.Morpheus.App.Internal.Resolving
  ( resultOr,
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
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString)

importGQLDocument :: FilePath -> Q [Dec]
importGQLDocument src = do
  qAddDependentFile src
  runIO (readFile src)
    >>= compileDocument
      ServerDecContext
        { namespace = False
        }

importGQLDocumentWithNamespace :: FilePath -> Q [Dec]
importGQLDocumentWithNamespace src = do
  qAddDependentFile src
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
