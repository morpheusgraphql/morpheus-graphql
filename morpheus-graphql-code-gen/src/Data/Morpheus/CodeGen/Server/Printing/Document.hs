{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.Document
  ( renderDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( ModuleDefinition (..),
    ServerDeclaration (..),
  )
import Data.Text
  ( pack,
  )
import qualified Data.Text.Lazy as LT
  ( fromStrict,
  )
import Data.Text.Lazy.Encoding (encodeUtf8)
import Prettyprinter (pretty)
import Relude hiding (ByteString, encodeUtf8)

renderDocument :: String -> [ServerDeclaration] -> ByteString
renderDocument moduleName types =
  encodeUtf8 $
    LT.fromStrict $
      show $
        pretty
          ModuleDefinition
            { moduleName = pack moduleName,
              imports =
                [ ("Data.Data", ["Typeable"]),
                  ("Data.Morpheus.Kind", ["TYPE"]),
                  ("Data.Morpheus.Types", ["*"]),
                  ("Data.Morpheus", []),
                  ("Data.Text", ["Text"]),
                  ("GHC.Generics", ["Generic"]),
                  ("Globals.GQLScalars", ["*"])
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
