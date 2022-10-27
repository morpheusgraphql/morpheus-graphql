{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Document
  ( gqlDocument,
    importGQLDocument,
    importGQLDocumentWithNamespace,
    RootResolverConstraint,
  )
where

import Data.Morpheus.CodeGen.Server
  ( CodeGenConfig (..),
    gqlDocument,
    importServerTypeDefinitions,
  )
import Data.Morpheus.Server (RootResolverConstraint)
import Language.Haskell.TH
import Relude hiding (ByteString, readFile)

importGQLDocument :: FilePath -> Q [Dec]
importGQLDocument = importServerTypeDefinitions CodeGenConfig {namespace = False}

importGQLDocumentWithNamespace :: FilePath -> Q [Dec]
importGQLDocumentWithNamespace = importServerTypeDefinitions CodeGenConfig {namespace = True}
