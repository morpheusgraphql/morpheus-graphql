{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Compile
  ( compileDocument,
    gqlDocument,
  )
where

--
--  Morpheus

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen
  ( parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConfig (..),
  )
import Data.Morpheus.Server.TH.Declare
  ( runDeclare,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Relude hiding (ByteString)

gqlDocument :: QuasiQuoter
gqlDocument = mkQuasiQuoter CodeGenConfig {namespace = False}

mkQuasiQuoter :: CodeGenConfig -> QuasiQuoter
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

compileDocument :: CodeGenConfig -> ByteString -> Q [Dec]
compileDocument ctx = parseServerTypeDefinitions ctx >=> runDeclare ctx
