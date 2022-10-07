{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.Render
  ( renderDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( ModuleDefinition (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.CodeGen.Server.Printing.Type
  ( renderTypes,
  )
import Data.Morpheus.CodeGen.Utils
  ( renderExtension,
    renderImport,
  )
import Data.Text
  ( pack,
  )
import qualified Data.Text.Lazy as LT
  ( fromStrict,
  )
import Data.Text.Lazy.Encoding (encodeUtf8)
import Prettyprinter
  ( Doc,
    line,
    pretty,
    vsep,
    (<+>),
  )
import Relude hiding (ByteString, encodeUtf8)

renderDocument :: String -> [ServerTypeDefinition] -> ByteString
renderDocument moduleName types =
  encodeUtf8 $
    LT.fromStrict $
      pack $
        show $
          renderModuleDefinition
            ModuleDefinition
              { moduleName = pack moduleName,
                imports =
                  [ ("Data.Data", ["Typeable"]),
                    ("Data.Morpheus.Kind", ["TYPE"]),
                    ("Data.Morpheus.Types", ["*"]),
                    ("Data.Morpheus", []),
                    ("Data.Text", ["Text"]),
                    ("GHC.Generics", ["Generic"])
                  ],
                extensions =
                  [ "DeriveGeneric",
                    "TypeFamilies",
                    "OverloadedStrings",
                    "DataKinds",
                    "DuplicateRecordFields"
                  ],
                types
              }

renderModuleDefinition :: ModuleDefinition -> Doc n
renderModuleDefinition
  ModuleDefinition
    { extensions,
      moduleName,
      imports,
      types
    } =
    vsep (map renderExtension extensions)
      <> line
      <> line
      <> "module"
      <+> pretty moduleName
      <+> "where"
        <> line
        <> line
        <> vsep (map renderImport imports)
        <> line
        <> line
        <> either (error . show) id (renderTypes types)
