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
    readFile,
  )
import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus.CodeGen.Server
  ( CodeGenConfig (..),
    compileDocument,
    gqlDocument,
  )
import Data.Morpheus.Server
  ( RootResolverConstraint,
    printSchema,
  )
import Data.Morpheus.Types (RootResolver)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString, readFile)

importDeclarations :: CodeGenConfig -> FilePath -> Q [Dec]
importDeclarations ctx rawSrc = do
  src <- makeRelativeToProject rawSrc
  qAddDependentFile src
  runIO (readFile src)
    >>= compileDocument ctx

importGQLDocument :: FilePath -> Q [Dec]
importGQLDocument = importDeclarations CodeGenConfig {namespace = False}

importGQLDocumentWithNamespace :: FilePath -> Q [Dec]
importGQLDocumentWithNamespace = importDeclarations CodeGenConfig {namespace = True}

{-# DEPRECATED toGraphQLDocument "use Data.Morpheus.Server.printSchema" #-}
toGraphQLDocument ::
  RootResolverConstraint m event query mut sub =>
  proxy (RootResolver m event query mut sub) ->
  ByteString
toGraphQLDocument = printSchema
