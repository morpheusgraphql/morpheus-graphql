{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    renderObject,
    renderMembers,
    newline,
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

indent :: Rendering
indent = "  "

space :: Rendering
space = " "

newline :: Rendering
newline = "\n"

indentNewline :: Rendering
indentNewline = newline <> indent

renderAtNewLine :: (RenderGQL a) => [a] -> Rendering
renderAtNewLine elems = indentNewline <> intercalate indentNewline (fmap render elems)

renderObject :: (RenderGQL a) => [a] -> Rendering
renderObject fields = " {" <> renderAtNewLine fields <> "\n}"

renderMembers :: (RenderGQL a) => [a] -> Rendering
renderMembers members = intercalate (space <> "|" <> space) (fmap render members)
