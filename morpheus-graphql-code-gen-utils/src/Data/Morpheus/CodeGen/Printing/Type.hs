{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Printing.Type
  ( RenderType (..),
    renderTypeRef,
    renderDeriving,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( DerivingClass (..),
  )
import Data.Morpheus.CodeGen.Printing.Terms
  ( TypeDoc (TypeDoc),
    renderName,
    renderWrapped,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeRef (..),
  )
import Prettyprinter
  ( Doc,
    pretty,
    tupled,
    (<+>),
  )
import Relude hiding (show)

type Result = Either Text

class RenderType a where
  render :: a -> Result (Doc ann)

renderDeriving :: [DerivingClass] -> Doc n
renderDeriving = ("deriving" <+>) . tupled . map pretty

renderTypeRef :: TypeRef -> TypeDoc n
renderTypeRef
  TypeRef
    { typeConName,
      typeWrappers
    } =
    renderWrapped
      typeWrappers
      (TypeDoc False (renderName typeConName))
