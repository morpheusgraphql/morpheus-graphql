{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Render.Terms
  ( indent
  , renderData
  , renderCon
  , renderMaybe
  , renderList
  , renderTuple
  , renderAssignment
  ) where

import           Data.Text (Text)

indent :: Text
indent = "  "

renderData :: Text -> Text
renderData name = "data " <> name <> " = "

renderCon :: Text -> Text
renderCon name = name <> " "

renderMaybe :: Text -> Text
renderMaybe typeName = "Maybe " <> typeName

renderList :: Text -> Text
renderList typeName = "[" <> typeName <> "]"

renderTuple :: Text -> Text
renderTuple typeName = "(" <> typeName <> ")"

renderAssignment :: Text -> Text -> Text
renderAssignment key value = key <> " :: " <> value
