{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
  )

type Rendering = Text

class RenderGQL a where
  render :: a -> Rendering

renderIndent :: Rendering
renderIndent = "  "

renderObject :: (a -> Rendering) -> [a] -> Rendering
renderObject f list =
  " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n}"
