{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    renderObject,
    renderMembers,
    newline,
    renderArguments,
    renderEntry,
    space,
    Rendering,
  )
where

-- MORPHEUS

import qualified Data.Aeson as A
import Data.Foldable (null)
import Data.Functor ((<$>))
import Data.Maybe (Maybe, maybe)
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    intercalate,
    pack,
  )
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Prelude
  ( (.),
    ($),
    Bool (..),
    Float,
    Int,
    fmap,
    otherwise,
    show,
  )

type Rendering = Text

class RenderGQL a where
  render :: a -> Rendering

instance
  RenderGQL a =>
  RenderGQL (Maybe a)
  where
  render = maybe "" render

instance RenderGQL Int where
  render = pack . show

instance RenderGQL Float where
  render = pack . show

instance RenderGQL Text where
  render = pack . show

instance RenderGQL Bool where
  render True = "true"
  render False = "false"

instance RenderGQL A.Value where
  render x = decodeUtf8 $ toStrict $ A.encode x

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

renderArguments :: (RenderGQL a) => [a] -> Rendering
renderArguments arguments
  | null arguments = ""
  | otherwise = "(" <> intercalate ", " (render <$> arguments) <> ")"

renderEntry ::
  (RenderGQL name, RenderGQL value) =>
  name ->
  value ->
  Rendering
renderEntry name value = render name <> ": " <> render value
