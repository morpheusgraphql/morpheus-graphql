{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Printer
  ( HSDoc,
    Printer (..),
    apply,
    infix',
    printDoc,
    renderDeriving,
    unpackHSDoc,
    wrapped,
    (.<>),
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
    list,
    pretty,
    tupled,
    (<+>),
  )
import Relude hiding (print, show)

renderDeriving :: [DerivingClass] -> Doc n
renderDeriving = ("deriving" <+>) . tupled . map pretty

infix' :: HSDoc n -> HSDoc n -> HSDoc n -> HSDoc n
infix' a op b = liftDoc $ rawDocument a <+> rawDocument op <+> rawDocument b

(.<>) :: HSDoc n -> HSDoc n -> HSDoc n
(.<>) a b = HSDoc True $ unpackHSDoc a <+> unpackHSDoc b

apply :: Name t -> [HSDoc n] -> HSDoc n
apply name xs = HSDoc True (foldl' (\b a -> b <+> unpackHSDoc a) (printDoc name) xs)

renderMaybe :: Bool -> HSDoc n -> HSDoc n
renderMaybe True = id
renderMaybe False = (.<>) "Maybe"

renderList :: HSDoc n -> HSDoc n
renderList = liftDoc . list . pure . rawDocument

liftDoc :: Doc n -> HSDoc n
liftDoc = HSDoc False

printDoc :: Printer a => a -> Doc n
printDoc = unpackHSDoc . print

unpackHSDoc :: HSDoc n -> Doc n
unpackHSDoc HSDoc {..} = if isComplex then tupled [rawDocument] else rawDocument

data HSDoc n = HSDoc
  { isComplex :: Bool,
    rawDocument :: Doc n
  }

class Printer a where
  print :: a -> HSDoc ann

instance IsString (HSDoc n) where
  fromString = liftDoc . pretty

instance Printer TypeRef where
  print TypeRef {..} = wrapped typeWrappers (print typeConName)

wrapped :: TypeWrapper -> HSDoc n -> HSDoc n
wrapped (TypeList wrapper notNull) = renderMaybe notNull . renderList . wrapped wrapper
wrapped (BaseType notNull) = renderMaybe notNull

instance Printer (Name t) where
  print = liftDoc . pretty . T.unpack . unpackName

instance Printer Text where
  print = liftDoc . pretty

instance Printer String where
  print = liftDoc . pretty
