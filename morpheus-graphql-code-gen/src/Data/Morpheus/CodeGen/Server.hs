{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server
  ( printServerTypeDefinitions,
    PrinterConfig (..),
    CodeGenConfig (..),
    compileDocument,
    gqlDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    ServerTypeDefinition,
  )
import Data.Morpheus.CodeGen.Server.Printing.Render
  ( renderDocument,
  )
import Data.Morpheus.CodeGen.Server.TH.Compile (compileDocument, gqlDocument)
import Relude hiding (ByteString)

newtype PrinterConfig = PrinterConfig
  { moduleName :: String
  }

printServerTypeDefinitions :: PrinterConfig -> [ServerTypeDefinition] -> ByteString
printServerTypeDefinitions PrinterConfig {moduleName} = renderDocument moduleName
