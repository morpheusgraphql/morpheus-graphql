{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Printing.Terms
  ( renderExtension,
    renderWrapped,
    label,
    renderName,
    parametrizedType,
    TypeDoc (..),
    appendType,
    optional,
    renderImport,
    renderType,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( TypeName,
    TypeWrapper (..),
    unpackName,
  )
import Data.Morpheus.CodeGen.Internal.Name (toHaskellTypeName)
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    hsep,
    list,
    pretty,
    tupled,
  )
import Relude hiding (optional)

parametrizedType :: TypeName -> [TypeName] -> Doc ann
parametrizedType tName typeParameters = hsep $ map renderName (tName : typeParameters)

-- TODO: this should be done in transformer
renderName :: TypeName -> Doc ann
renderName = pretty . toHaskellTypeName

renderExtension :: Text -> Doc ann
renderExtension name = "{-#" <+> "LANGUAGE" <+> pretty name <+> "#-}"

data TypeDoc n = TypeDoc
  { isComplex :: Bool,
    unDoc :: Doc n
  }

renderType :: TypeDoc n -> Doc n
renderType TypeDoc {isComplex, unDoc = doc} = if isComplex then tupled [doc] else doc

appendType :: TypeName -> TypeDoc n -> TypeDoc n
appendType t1 tyDoc = TypeDoc True $ renderName t1 <> " " <> renderType tyDoc

renderMaybe :: Bool -> TypeDoc n -> TypeDoc n
renderMaybe True = id
renderMaybe False = appendType "Maybe"

renderList :: TypeDoc n -> TypeDoc n
renderList = TypeDoc False . list . pure . unDoc

renderWrapped :: TypeWrapper -> TypeDoc n -> TypeDoc n
renderWrapped (TypeList wrapper notNull) = renderMaybe notNull . renderList . renderWrapped wrapper
renderWrapped (BaseType notNull) = renderMaybe notNull

label :: TypeName -> Doc ann
label typeName = "---- GQL " <> pretty (unpackName typeName) <> " -------------------------------\n"

optional :: ([a] -> Doc n) -> [a] -> Doc n
optional _ [] = ""
optional f xs = " " <> f xs

renderImport :: (Text, [Text]) -> Doc ann
renderImport (src, ls) =
  "import" <+> pretty src
    <> optional (tupled . map pretty) ls
