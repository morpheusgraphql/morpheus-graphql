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

import Data.ByteString.Lazy.Char8 (ByteString, readFile)
import Data.Morpheus.Server
import Data.Morpheus.Server.CodeGen.Types
  ( ServerDecContext (..),
  )
import Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
  )
import Data.Morpheus.Server.TH.Compile
  ( compileDocument,
    gqlDocument,
  )
import Data.Morpheus.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString, readFile)

importGQLDocument :: FilePath -> Q [Dec]
importGQLDocument =
  importDeclarations ServerDecContext {namespace = False}

importGQLDocumentWithNamespace :: FilePath -> Q [Dec]
importGQLDocumentWithNamespace =
  importDeclarations ServerDecContext {namespace = True}

importDeclarations :: ServerDecContext -> FilePath -> Q [Dec]
importDeclarations ctx src = do
  qAddDependentFile src
  runIO (readFile src)
    >>= compileDocument ctx

-- TODO: deprecate
toGraphQLDocument ::
  RootResolverConstraint m event query mut sub =>
  proxy (RootResolver m event query mut sub) ->
  ByteString
toGraphQLDocument = printSchema
