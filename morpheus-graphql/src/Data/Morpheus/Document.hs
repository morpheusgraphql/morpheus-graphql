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

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Server
  ( CodeGenConfig (..),
    gqlDocument,
    importServerTypeDefinitions,
  )
import Data.Morpheus.Server
  ( RootResolverConstraint,
    printSchema,
  )
import Data.Morpheus.Types (RootResolver)
import Language.Haskell.TH
import Relude hiding (ByteString, readFile)

importGQLDocument :: FilePath -> Q [Dec]
importGQLDocument = importServerTypeDefinitions CodeGenConfig {namespace = False}

importGQLDocumentWithNamespace :: FilePath -> Q [Dec]
importGQLDocumentWithNamespace = importServerTypeDefinitions CodeGenConfig {namespace = True}

{-# DEPRECATED toGraphQLDocument "use Data.Morpheus.Server.printSchema" #-}
toGraphQLDocument ::
  RootResolverConstraint m event query mut sub =>
  proxy (RootResolver m event query mut sub) ->
  ByteString
toGraphQLDocument = printSchema
