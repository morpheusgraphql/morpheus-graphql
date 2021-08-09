{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering.Render
  ( renderHaskellDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
-- MORPHEUS

import Data.HashMap.Lazy
  ( toList,
  )
import Data.Morpheus.Types.Internal.AST
  ( DataType,
    DataTypeLib (..),
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    pack,
  )
import qualified Data.Text.Lazy as LT
  ( fromStrict,
  )
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    cat,
    comma,
    encloseSep,
    line,
    lparen,
    pretty,
    rparen,
    vsep,
  )
import Rendering.Terms
  ( Context (..),
    renderExtension,
  )
import Rendering.Types (renderType)

renderHaskellDocument :: String -> DataTypeLib -> ByteString
renderHaskellDocument modName lib =
  encodeUtf8
    $ LT.fromStrict
    $ pack
    $ show
    $ renderLanguageExtensions context
      <> renderExports context
      <> renderImports context
      <> renderTypes
  where
    renderTypes = cat $ map renderFullType ts
      where
        ts :: [(Text, DataType)]
        ts = toList (types lib)
        renderFullType :: (Text, DataType) -> Doc ann
        renderFullType = renderType context
    context =
      Context
        { moduleName = pack modName,
          imports =
            [ ( "Data.Morpheus.Kind",
                ["SCALAR", "ENUM", "INPUT", "OBJECT", "UNION"]
              ),
              ( "Data.Morpheus.Types",
                [ "GQLType (..)",
                  "GQLScalar (..)",
                  "ScalarValue (..)"
                ]
              ),
              ("Data.Text", ["Text"]),
              ("GHC.Generics", ["Generic"])
            ],
          extensions =
            [ "DeriveAnyClass",
              "DeriveGeneric",
              "TypeFamilies"
            ],
          schema = lib
        }

renderLanguageExtensions :: Context -> Doc ann
renderLanguageExtensions Context {extensions} =
  vsep (map renderExtension extensions)
    <> line
    <> line

renderExports :: Context -> Doc ann
renderExports Context {moduleName} =
  "module"
    <+> pretty moduleName
    <+> "where"
    <> line
    <> line

renderImports :: Context -> Doc ann
renderImports Context {imports} = vsep (map renderImport imports) <> line
  where
    renderImport (src, ls) = "import" <+> pretty src <+> sepMap ls

sepMap :: [Text] -> Doc ann
sepMap ls = encloseSep lparen rparen comma (map pretty ls)
