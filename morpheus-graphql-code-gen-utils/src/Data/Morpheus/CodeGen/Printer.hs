{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Printer
  ( HSDoc,
    Printer (..),
    apply,
    infix',
    print',
    renderDeriving,
    unpack,
    wrapped,
    (.<>),
    optional,
    parametrizedType,
    renderExtension,
    renderImport,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( DerivingClass (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Name,
    TypeRef (..),
    TypeWrapper (..),
    unpackName,
  )
import qualified Data.Text as T
import Prettyprinter
  ( Doc,
    Pretty (..),
    hsep,
    list,
    pretty,
    tupled,
    (<+>),
  )
import Relude hiding (optional, print, show)

renderDeriving :: [DerivingClass] -> Doc n
renderDeriving = ("deriving" <+>) . tupled . map pretty

infix' :: HSDoc n -> HSDoc n -> HSDoc n -> HSDoc n
infix' a op b = pack $ rawDocument a <+> rawDocument op <+> rawDocument b

(.<>) :: HSDoc n -> HSDoc n -> HSDoc n
(.<>) a b = HSDoc True $ unpack a <+> unpack b

apply :: Name t -> [HSDoc n] -> HSDoc n
apply name xs = HSDoc True (foldl' (\b a -> b <+> unpack a) (print' name) xs)

renderMaybe :: Bool -> HSDoc n -> HSDoc n
renderMaybe True = id
renderMaybe False = (.<>) "Maybe"

renderList :: HSDoc n -> HSDoc n
renderList = pack . list . pure . rawDocument

print' :: Printer a => a -> Doc n
print' = unpack . print

pack :: Doc n -> HSDoc n
pack = HSDoc False

unpack :: HSDoc n -> Doc n
unpack HSDoc {..} = if isComplex then tupled [rawDocument] else rawDocument

data HSDoc n = HSDoc
  { isComplex :: Bool,
    rawDocument :: Doc n
  }

class Printer a where
  print :: a -> HSDoc ann

instance IsString (HSDoc n) where
  fromString = pack . pretty

instance Printer TypeRef where
  print TypeRef {..} = wrapped typeWrappers (print typeConName)

wrapped :: TypeWrapper -> HSDoc n -> HSDoc n
wrapped (TypeList wrapper notNull) = renderMaybe notNull . renderList . wrapped wrapper
wrapped (BaseType notNull) = renderMaybe notNull

instance Printer (Name t) where
  print = pack . pretty . T.unpack . unpackName

instance Printer Text where
  print = pack . pretty

instance Printer String where
  print = pack . pretty

optional :: ([a] -> Doc n) -> [a] -> Doc n
optional _ [] = ""
optional f xs = " " <> f xs

parametrizedType :: Text -> [Text] -> Doc ann
parametrizedType tName typeParameters = hsep $ map pretty $ tName : typeParameters

renderExtension :: Text -> Doc ann
renderExtension name = "{-#" <+> "LANGUAGE" <+> pretty name <+> "#-}"

renderImport :: (Text, [Text]) -> Doc ann
renderImport (src, ls) = "import" <+> pretty src <> renderImportList ls

renderImportList :: [Text] -> Doc ann
renderImportList ["*"] = ""
renderImportList xs = tupled (map pretty xs)
