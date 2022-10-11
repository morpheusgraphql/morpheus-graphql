{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    renderExtension,
    renderImport,
    ignore,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    CodeGenTypeName (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    getFullName,
  )
import Data.Morpheus.Types.Internal.AST
  ( Name,
    TypeRef (..),
    TypeWrapper (..),
    unpackName,
  )
import qualified Data.Text as T
import Prettyprinter (Doc, Pretty (..), comma, enclose, hsep, indent, line, list, nest, pretty, punctuate, tupled, vsep, (<+>))
import Relude hiding (optional, print, show)
import Prelude (show)

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

ignore :: HSDoc n -> Doc n
ignore HSDoc {..} = rawDocument

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

renderExtension :: Text -> Doc ann
renderExtension name = "{-#" <+> "LANGUAGE" <+> pretty name <+> "#-}"

renderImport :: (Text, [Text]) -> Doc ann
renderImport (src, ls) = "import" <+> pretty src <> renderImportList ls

renderImportList :: [Text] -> Doc ann
renderImportList ["*"] = ""
renderImportList xs = tupled (map pretty xs)

renderWrapper :: FIELD_TYPE_WRAPPER -> HSDoc n -> HSDoc n
renderWrapper PARAMETRIZED = (.<> "m")
renderWrapper MONAD = ("m" .<>)
renderWrapper SUBSCRIPTION {} = id
renderWrapper (GQL_WRAPPER typeWrappers) = wrapped typeWrappers
renderWrapper (ARG name) = infix' (print name) "->"
renderWrapper (TAGGED_ARG _ name typeRef) = infix' (apply "Arg" [print (show name), print typeRef]) "->"

instance Printer CodeGenField where
  print CodeGenField {..} = infix' (print fieldName) "::" (foldr renderWrapper (print fieldType) wrappers)

instance Printer CodeGenConstructor where
  print CodeGenConstructor {constructorName, constructorFields = []} =
    print constructorName
  print CodeGenConstructor {constructorName, constructorFields} = do
    let fields = map (unpack . print) constructorFields
    pack (print' constructorName <> renderSet fields)
    where
      renderSet = nest 2 . enclose "\n{ " "\n}" . nest 2 . vsep . punctuate comma

instance Printer CodeGenTypeName where
  print cgName =
    HSDoc (not $ null (typeParameters cgName)) $
      parametrizedType
        (unpackName (getFullName cgName))
        (typeParameters cgName)

parametrizedType :: Text -> [Text] -> Doc ann
parametrizedType tName typeParameters = hsep $ map pretty $ tName : typeParameters

instance Pretty CodeGenType where
  pretty CodeGenType {..} =
    "data"
      <+> ignore (print cgTypeName)
        <> renderConstructors cgConstructors
        <> line
        <> indent 2 (renderDeriving cgDerivations)
        <> line
    where
      renderConstructors [cons] = (" =" <+>) $ print' cons
      renderConstructors conses = nest 2 . (line <>) . vsep . prefixVariants $ map print' conses
      prefixVariants (x : xs) = "=" <+> x : map ("|" <+>) xs
      prefixVariants [] = []
