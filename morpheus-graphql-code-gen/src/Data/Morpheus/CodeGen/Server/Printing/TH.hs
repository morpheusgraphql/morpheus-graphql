{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.TH
  ( compileDocument,
    gqlDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    InterfaceDefinition (..),
    ServerDeclaration (..),
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Transform
  ( parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.TH
  ( apply,
    m',
    m_,
    printDec,
    printTypeSynonym,
  )
import Data.Morpheus.Server.Types
  ( TypeGuard (..),
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
compileDocument ctx = parseServerTypeDefinitions ctx >=> printDecQ

class PrintDecQ a where
  printDecQ :: a -> Q [Dec]

instance PrintDecQ a => PrintDecQ [a] where
  printDecQ = fmap concat . traverse printDecQ

instance PrintDecQ InterfaceDefinition where
  printDecQ InterfaceDefinition {..} =
    pure [printTypeSynonym aliasName [m_] (apply ''TypeGuard [apply interfaceName [m'], apply unionName [m']])]

instance PrintDecQ ServerDeclaration where
  printDecQ (InterfaceType interface) = printDecQ interface
  printDecQ ScalarType {} = pure []
  printDecQ (DataType dataType) = pure <$> printDec dataType
  printDecQ (GQLTypeInstance _ gql) = pure <$> printDec gql
  printDecQ (GQLDirectiveInstance _ dir) = pure <$> printDec dir
