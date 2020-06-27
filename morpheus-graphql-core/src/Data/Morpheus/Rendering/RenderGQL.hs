{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    renderObject,
    renderIndent,
  )
where

-- MORPHEUS
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    intercalate,
    pack,
  )
import Prelude
  ( (.),
    Bool (..),
    Float,
    Int,
    fmap,
    show,
  )

type Rendering = Text

class RenderGQL a where
  render :: a -> Rendering

instance RenderGQL Int where
  render = pack . show

instance RenderGQL Float where
  render = pack . show

instance RenderGQL Text where
  render = pack . show

instance RenderGQL Bool where
  render True = "true"
  render False = "false"

renderIndent :: Rendering
renderIndent = "  "

renderObject :: (a -> Rendering) -> [a] -> Rendering
renderObject f list =
  " { \n  " <> intercalate ("\n" <> renderIndent) (fmap f list) <> "\n}"
