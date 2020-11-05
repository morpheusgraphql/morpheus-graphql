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
    fromText,
    renderGQL,
    intercalate,
    renderInputSeq,
  )
where

-- MORPHEUS

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Foldable (foldl)
import qualified Data.Text as T
import Relude hiding
  ( ByteString,
    intercalate,
  )

renderGQL :: RenderGQL a => a -> ByteString
renderGQL x = runRendering (render x) 0

newtype Rendering = Rendering
  { runRendering :: Int -> ByteString
  }

instance Semigroup Rendering where
  Rendering f <> Rendering g = Rendering $ \x -> f x <> g x

instance IsString Rendering where
  fromString = Rendering . const . LB.pack

fromShow :: Show a => a -> Rendering
fromShow = fromString . show

fromText :: Text -> Rendering
fromText = fromString . T.unpack

class RenderGQL a where
  render :: a -> Rendering

instance
  RenderGQL a =>
  RenderGQL (Maybe a)
  where
  render = maybe "" render

instance RenderGQL ByteString where
  render = Rendering . const

instance RenderGQL Int where
  render = fromShow

instance RenderGQL Float where
  render = fromShow

instance RenderGQL Text where
  render = fromShow

instance RenderGQL Bool where
  render True = "true"
  render False = "false"

instance RenderGQL A.Value where
  render = render . A.encode

space :: Rendering
space = " "

newline :: Rendering
newline = "\n" <> Rendering indentionSize

indentionSize :: (Semigroup a, IsString a) => Int -> a
indentionSize 0 = ""
indentionSize n = stimes (n * 2) " "

indent :: Rendering -> Rendering
indent (Rendering f) = Rendering $ f . (+ 1)

intercalate :: Rendering -> [Rendering] -> Rendering
intercalate (Rendering f) fs = Rendering $ \x -> LB.intercalate (f x) (map ((x &) . runRendering) fs)

indentNewline :: Rendering -> Rendering
indentNewline rendering = indent (newline <> rendering)

renderAtNewLine :: (RenderGQL a) => [a] -> Rendering
renderAtNewLine elems = indentNewline $ intercalate newline (fmap render elems)

renderObject :: (RenderGQL a) => [a] -> Rendering
renderObject fields = space <> "{" <> renderAtNewLine fields <> newline <> "}"

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

renderInputSeq ::
  (Foldable t, RenderGQL a) =>
  t a ->
  Rendering
renderInputSeq = fromMaybe "" . foldl renderValue Nothing
  where
    renderValue :: RenderGQL a => Maybe Rendering -> a -> Maybe Rendering
    renderValue Nothing value = Just (render value)
    renderValue (Just txt) value = Just (txt <> ", " <> render value)
