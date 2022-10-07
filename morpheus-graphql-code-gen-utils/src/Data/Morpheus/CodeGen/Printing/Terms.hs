{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Printing.Terms
  ( renderExtension,
    label,
    parametrizedType,
    optional,
    renderImport,
  )
where

import Prettyprinter
  ( Doc,
    hsep,
    pretty,
    tupled,
    (<+>),
  )
import Relude hiding (optional)

parametrizedType :: Text -> [Text] -> Doc ann
parametrizedType tName typeParameters = hsep $ map pretty $ tName : typeParameters

renderExtension :: Text -> Doc ann
renderExtension name = "{-#" <+> "LANGUAGE" <+> pretty name <+> "#-}"

label :: Text -> Doc ann
label typeName = "---- GQL " <> pretty typeName <> " -------------------------------\n"

optional :: ([a] -> Doc n) -> [a] -> Doc n
optional _ [] = ""
optional f xs = " " <> f xs

renderImport :: (Text, [Text]) -> Doc ann
renderImport (src, ls) = "import" <+> pretty src <> renderImportList ls

renderImportList :: [Text] -> Doc ann
renderImportList ["*"] = ""
renderImportList xs = tupled (map pretty xs)
