{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server
  ( printServerTypeDefinitions,
    PrinterConfig (..),
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Internal.AST
  ( ServerTypeDefinition,
  )
import Data.Morpheus.CodeGen.Printing.Render
  ( renderDocument,
  )
import Relude hiding (ByteString)

newtype PrinterConfig = PrinterConfig
  { moduleName :: String
  }

printServerTypeDefinitions :: PrinterConfig -> [ServerTypeDefinition] -> ByteString
printServerTypeDefinitions PrinterConfig {moduleName} = renderDocument moduleName
