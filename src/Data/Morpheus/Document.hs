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

import Data.Morpheus.Server
import Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDecContext (..),
  )
import Data.Morpheus.Server.TH.Compile
  ( compileDocument,
    gqlDocument,
  )
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
