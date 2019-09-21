{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Rendering.Haskell.Terms
  ( indent
  , renderReturn
  , renderData
  , renderCon
  , renderMaybe
  , renderList
  , renderTuple
  , renderAssignment
  , renderExtension
  , renderWrapped
  , renderSet
  , renderUnionCon
  , renderEqual
  , Scope(..)
  , Context(..)
  ) where

import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text, intercalate, toUpper)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data (WrapperD (..))

indent :: Text
indent = "  "

renderEqual :: Text -> Text -> Text
renderEqual key value = key <> " = " <> value

renderReturn :: Text
renderReturn = "return "

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

renderSet :: [Text] -> Text
renderSet fields = bracket "{ " <> intercalate ("\n  ," <> indent) fields <> bracket "}\n"
  where
    bracket x = "\n    " <> x

renderAssignment :: Text -> Text -> Text
renderAssignment key value = key <> " :: " <> value

renderExtension :: Text -> Text
renderExtension name = "{-# LANGUAGE " <> name <> " #-}\n"

renderWrapped :: [WrapperD] -> Text -> Text
renderWrapped (ListD:xs)  = renderList . renderWrapped xs
renderWrapped (MaybeD:xs) = renderMaybe . renderWrapped xs
renderWrapped []          = strToText

strToText :: Text -> Text
strToText "String" = "Text"
strToText x        = x

renderUnionCon :: Text -> Text -> Text
renderUnionCon typeName conName = renderCon (typeName <> "_" <> toUpper conName)

data Scope
  = Mutation
  | Subscription
  | Query

data Context = Context
  { moduleName :: Text
  , imports    :: [(Text, [Text])]
  , extensions :: [Text]
  , scope      :: Scope
  , pubSub     :: (Text, Text)
  }
