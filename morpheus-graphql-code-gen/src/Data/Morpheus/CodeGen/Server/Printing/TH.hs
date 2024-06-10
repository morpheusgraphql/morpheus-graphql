{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.TH
  ( compileDocument,
    gqlDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    ServerDeclaration (..),
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Transform
  ( parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintDec (..),
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Relude hiding (ByteString, Type)

gqlDocument :: QuasiQuoter
gqlDocument = mkQuasiQuoter CodeGenConfig {namespace = False}

mkQuasiQuoter :: CodeGenConfig -> QuasiQuoter
mkQuasiQuoter ctx =
  QuasiQuoter
    { quoteExp = notHandled "Expressions",
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = compileDocument ctx . pack
    }
  where
    notHandled things =
      error $ things <> " are not supported by the GraphQL QuasiQuoter"

compileDocument :: CodeGenConfig -> ByteString -> Q [Dec]
compileDocument config =
  parseServerTypeDefinitions config
    >=> fmap concat
    . traverse printServerDec
    . fst

printServerDec :: ServerDeclaration -> Q [Dec]
printServerDec (InterfaceType interface) = pure <$> printDec interface
printServerDec ScalarType {} = pure []
printServerDec (DataType dataType) = pure <$> printDec dataType
printServerDec (GQLTypeInstance _ gql) = pure <$> printDec gql
printServerDec (GQLDirectiveInstance dir) = pure <$> printDec dir
