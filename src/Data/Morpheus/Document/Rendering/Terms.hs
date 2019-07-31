{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Rendering.Terms
  ( indent
  , renderData
  , renderCon
  , renderMaybe
  , renderList
  , renderTuple
  , renderAssignment
  , renderExtension
  , renderWrapped
  ) where

import           Data.Morpheus.Types.Internal.Data (DataTypeWrapper (..))
import           Data.Text                         (Text)

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

renderExtension :: Text -> Text
renderExtension name = "{-# LANGUAGE " <> name <> " #-}\n"

renderWrapped :: [DataTypeWrapper] -> Text -> Text
renderWrapped [] typeName                          = renderMaybe typeName
renderWrapped [NonNullType] typeName               = typeName
renderWrapped (NonNullType:(ListType:xs)) typeName = renderList $ renderWrapped xs typeName
renderWrapped (ListType:xs) typeName               = renderMaybe $ renderList $ renderWrapped xs typeName
renderWrapped (NonNullType:xs) typeName            = renderWrapped xs typeName
