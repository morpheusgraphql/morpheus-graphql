{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Document
  ( gqlDocument,
    importGQLDocument,
    importGQLDocumentWithNamespace,
    RootResolverConstraint,
    toGraphQLDocument,
  )
where

import Data.ByteString.Lazy.Char8 (readFile)
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
