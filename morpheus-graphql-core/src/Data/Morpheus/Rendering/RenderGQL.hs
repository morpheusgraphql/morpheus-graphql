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
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (Foldable, foldl, null)
import Data.Function ((&))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Semigroup (Semigroup (..), stimes)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Prelude
  ( ($),
    (*),
    (+),
    (.),
    Bool (..),
    Float,
    Int,
    Show (..),
    const,
    fmap,
    map,
    otherwise,
  )

renderGQL :: RenderGQL a => a -> Text
renderGQL x = runRendering (render x) 0

newtype Rendering = Rendering
  { runRendering :: Int -> Text
  }

instance Semigroup Rendering where
  Rendering f <> Rendering g = Rendering $ \x -> f x <> g x

instance IsString Rendering where
  fromString = Rendering . const . T.pack

fromShow :: Show a => a -> Rendering
fromShow x = Rendering (const $ T.pack (show x))

fromText :: Text -> Rendering
fromText = Rendering . const

class RenderGQL a where
  render :: a -> Rendering

instance
  RenderGQL a =>
  RenderGQL (Maybe a)
  where
  render = maybe (fromText "") render

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
  render x = fromText $ decodeUtf8 $ toStrict $ A.encode x

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
intercalate (Rendering f) fs = Rendering $ \x -> T.intercalate (f x) (map ((x &) . runRendering) fs)

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
