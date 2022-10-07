{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server
  ( CodeGenConfig (..),
    PrinterConfig (..),
    gqlDocument,
    importServerTypeDefinitions,
    printServerTypeDefinitions,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    readFile,
  )
import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    ServerTypeDefinition,
  )
import Data.Morpheus.CodeGen.Server.Printing.Render
  ( renderDocument,
  )
import Data.Morpheus.CodeGen.Server.TH.Compile
  ( compileDocument,
    gqlDocument,
  )
import Language.Haskell.TH (Dec, Q, runIO)
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString, readFile)

newtype PrinterConfig = PrinterConfig
  { moduleName :: String
  }

printServerTypeDefinitions :: PrinterConfig -> [ServerTypeDefinition] -> ByteString
printServerTypeDefinitions PrinterConfig {moduleName} = renderDocument moduleName

importServerTypeDefinitions :: CodeGenConfig -> FilePath -> Q [Dec]
importServerTypeDefinitions ctx rawSrc = do
  src <- makeRelativeToProject rawSrc
  qAddDependentFile src
  runIO (readFile src)
    >>= compileDocument ctx
