{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Compile
  ( compileDocument,
    gqlDocument,
    gqlDocumentNamespace,
  )
where

--
--  Morpheus

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Server.CodeGen.Transform
  ( parseServerTypeDefinitions,
  )
import Data.Morpheus.Server.CodeGen.Types
  ( ServerDecContext (..),
  )
import Data.Morpheus.Server.TH.Declare
  ( runDeclare,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Relude hiding (ByteString)

gqlDocumentNamespace :: QuasiQuoter
gqlDocumentNamespace = mkQuasiQuoter ServerDecContext {namespace = True}

gqlDocument :: QuasiQuoter
gqlDocument = mkQuasiQuoter ServerDecContext {namespace = False}

mkQuasiQuoter :: ServerDecContext -> QuasiQuoter
mkQuasiQuoter ctx =
  QuasiQuoter
    { quoteExp = notHandled "Expressions",
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = compileDocument ctx . LB.pack
    }
  where
    notHandled things =
      error $ things <> " are not supported by the GraphQL QuasiQuoter"

compileDocument :: ServerDecContext -> ByteString -> Q [Dec]
compileDocument ctx = parseServerTypeDefinitions ctx >=> runDeclare ctx
