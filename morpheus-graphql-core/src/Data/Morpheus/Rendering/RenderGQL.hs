{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    render,
    renderObject,
    renderMembers,
    newline,
    renderArguments,
    renderEntry,
    space,
    Rendering,
    fromText,
    intercalate,
    renderInputSeq,
    unwords,
    fromShow,
    nonNillSpace,
  )
where

-- MORPHEUS

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Foldable (foldr')
import qualified Data.Text as T
import Relude hiding
  ( ByteString,
    intercalate,
    unwords,
  )

render :: (RenderGQL a) => a -> ByteString
render x = runRendering (renderGQL x) 0

newtype Rendering = Rendering
  { runRendering :: Int -> ByteString
  }

instance Semigroup Rendering where
  Rendering f <> Rendering g = Rendering $ \x -> f x <> g x

instance IsString Rendering where
  fromString = Rendering . const . LB.pack

fromShow :: (Show a) => a -> Rendering
fromShow = fromString . show

fromText :: Text -> Rendering
fromText = fromString . T.unpack

nonNillSpace :: (Foldable t) => t a -> Rendering
nonNillSpace t
  | null t = ""
  | otherwise = space

class RenderGQL a where
  renderGQL :: a -> Rendering

instance
  (RenderGQL a) =>
  RenderGQL (Maybe a)
  where
  renderGQL = maybe "" renderGQL

instance (RenderGQL l, RenderGQL r) => RenderGQL (Either l r) where
  renderGQL (Left x) = renderGQL x
  renderGQL (Right x) = renderGQL x

instance RenderGQL ByteString where
  renderGQL = Rendering . const

instance RenderGQL Int where
  renderGQL = fromShow

instance RenderGQL Float where
  renderGQL = fromShow

instance RenderGQL Double where
  renderGQL = fromShow

instance RenderGQL Text where
  renderGQL = fromShow

instance RenderGQL Bool where
  renderGQL True = "true"
  renderGQL False = "false"

instance RenderGQL A.Value where
  renderGQL = renderGQL . A.encode

space :: Rendering
space = " "

newline :: Rendering
newline = "\n" <> Rendering indentionSize

indentionSize :: (Semigroup a, IsString a) => Int -> a
indentionSize 0 = ""
indentionSize n = stimes (n * 2) " "

indent :: Rendering -> Rendering
indent (Rendering f) = Rendering $ f . (+ 1)

unwords :: [Rendering] -> Rendering
unwords = intercalate space

intercalate :: Rendering -> [Rendering] -> Rendering
intercalate (Rendering f) fs = Rendering $ \x -> LB.intercalate (f x) (map ((x &) . runRendering) fs)

indentNewline :: Rendering -> Rendering
indentNewline rendering = indent (newline <> rendering)

renderAtNewLine :: (RenderGQL a) => [a] -> Rendering
renderAtNewLine elems = indentNewline $ intercalate newline (fmap renderGQL elems)

renderObject :: (RenderGQL a) => [a] -> Rendering
renderObject fields = space <> "{" <> renderAtNewLine fields <> newline <> "}"

renderMembers :: (RenderGQL a, Foldable t) => t a -> Rendering
renderMembers members = intercalate (space <> "|" <> space) (fmap renderGQL (toList members))

renderArguments :: (RenderGQL a) => [a] -> Rendering
renderArguments arguments
  | null arguments = ""
  | otherwise = "(" <> intercalate ", " (renderGQL <$> arguments) <> ")"

renderEntry ::
  (RenderGQL name, RenderGQL value) =>
  name ->
  value ->
  Rendering
renderEntry name value = renderGQL name <> ": " <> renderGQL value

renderInputSeq ::
  (Foldable t, RenderGQL a) =>
  t a ->
  Rendering
renderInputSeq = fromMaybe "" . foldr' renderValue Nothing
  where
    renderValue :: (RenderGQL a) => a -> Maybe Rendering -> Maybe Rendering
    renderValue value Nothing = Just (renderGQL value)
    renderValue value (Just txt) = Just (renderGQL value <> ", " <> txt)
