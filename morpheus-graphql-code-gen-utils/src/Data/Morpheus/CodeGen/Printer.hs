{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Printer
  ( HSDoc (..),
    Printer (..),
    apply,
    infix',
    print',
    unpack,
    wrapped,
    (.<>),
    optional,
    ignore,
    pack,
  )
where

import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation,
    Name,
    TypeRef (..),
    TypeWrapper (..),
    packName,
    unpackName,
  )
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import Prettyprinter (Doc, Pretty (..), list, pretty, tupled, (<+>))
import Relude hiding (optional, print, show)
import Prelude (show)

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

print' :: (Printer a) => a -> Doc n
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

instance Printer DirectiveLocation where
  print = fromString . show

instance Printer TH.Name where
  print = print . packName

optional :: ([a] -> Doc n) -> [a] -> Doc n
optional _ [] = ""
optional f xs = " " <> f xs
