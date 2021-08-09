{-# LANGUAGE OverloadedStrings #-}

module Rendering.Terms
  ( indent,
    renderPure,
    renderData,
    renderCon,
    renderMaybe,
    renderList,
    renderTuple,
    renderAssignment,
    renderExtension,
    renderWrapped,
    renderSet,
    renderUnionCon,
    renderEqual,
    Context (..),
    ioRes,
    renderDeriving,
    renderInstanceHead,
    renderGQLTypeInstance,
    label,
    newline,
    double,
  )
where

-- MORPHEUS
-- MORPHEUS
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    Schema,
    TypeName,
    TypeWrapper (..),
    unpackName,
  )
import Data.Text
  ( Text,
    intercalate,
  )
import qualified Data.Text as T
  ( unwords,
  )
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    pretty,
  )

double :: Text -> Text
double x = x <> x

ioRes :: Text -> Text
ioRes event = "IORes " <> event <> " "

indent :: Text
indent = "  "

renderEqual :: Text -> Text -> Text
renderEqual key value = key <> " = " <> value

renderPure :: Text
renderPure = "pure "

newline :: Text
newline = "\n"

renderData :: Text -> [Text] -> Text
renderData name tyArgs =
  "data " <> T.unwords (name : tyArgs) <> " =" <> newline <> indent

renderCon :: Text -> Text
renderCon name = name <> " "

renderMaybe :: Text -> Text
renderMaybe typeName = "Maybe " <> typeName

renderList :: Text -> Text
renderList typeName = "[" <> typeName <> "]"

renderTuple :: Text -> Text
renderTuple typeName = "(" <> typeName <> ")"

renderSet :: [Text] -> Text
renderSet fields =
  bracket "{ " <> intercalate ("\n  ," <> indent) fields <> bracket "}\n"
  where
    bracket x = "\n    " <> x

renderAssignment :: Text -> Text -> Text
renderAssignment key value = key <> " :: " <> value

renderExtension :: Text -> Doc ann
renderExtension name = "{-#" <+> "LANGUAGE" <+> pretty name <+> "#-}"

renderWrapped :: [TypeWrapper] -> Text -> Text
renderWrapped (TypeList {} : xs) = renderList . renderWrapped xs
renderWrapped (BaseType {} : xs) = renderMaybe . renderWrapped xs
renderWrapped [] = strToText

strToText :: Text -> Text
strToText "String" = "Text"
strToText "Boolean" = "Bool"
strToText x = x

renderUnionCon :: Text -> Text -> Text
renderUnionCon typeName conName = renderCon (typeName <> conName)

renderGQLTypeInstance :: TypeName -> TypeName -> Text
renderGQLTypeInstance typeName kind =
  renderInstanceHead "GQLType" (unpackName typeName)
    <> indent
    <> "type KIND "
    <> unpackName typeName
    <> " = "
    <> unpackName kind
    <> newline

label :: TypeName -> Doc ann
label typeName =
  "\n---- GQL " <> pretty (unpackName typeName) <> " ------------------------------- \n"

renderInstanceHead :: Text -> Text -> Text
renderInstanceHead className name =
  "instance " <> className <> " " <> name <> " where\n"

renderDeriving :: [Text] -> Text
renderDeriving list =
  " deriving " <> renderTuple (intercalate ", " ("Generic" : list)) <> newline

data Context = Context
  { moduleName :: Text,
    imports :: [(Text, [Text])],
    extensions :: [Text],
    schema :: Schema CONST
  }
