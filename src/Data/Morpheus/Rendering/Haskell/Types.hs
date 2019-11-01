{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Rendering.Haskell.Types
  ( renderType
  ) where

import           Data.Maybe                            (catMaybes)
import           Data.Semigroup                        ((<>))
import           Data.Text                             (Text, intercalate, pack, toUpper)
import qualified Data.Text                             as T (head, tail)

-- MORPHEUS
import           Data.Morpheus.Rendering.Haskell.Terms (Context (..), Scope (..), indent, renderAssignment, renderCon,
                                                        renderData, renderSet, renderTuple, renderUnionCon,
                                                        renderWrapped)
import           Data.Morpheus.Types.Internal.Data     (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
                                                        DataTyCon (..), TypeAlias (..), isNullable)

renderType :: Context -> (Text, DataFullType) -> Text
renderType context (name, dataType) = typeIntro <> renderData name <> renderT dataType
  where
    renderT (Leaf (DataScalar _)) = renderCon name <> "Int Int" <> defineTypeClass "SCALAR" <> renderGQLScalar name
    renderT (Leaf (DataEnum DataTyCon {typeData})) = unionType typeData <> defineTypeClass "ENUM"
    renderT (Union DataTyCon {typeData}) = renderUnion name typeData <> defineTypeClass "UNION"
    renderT (InputObject DataTyCon {typeData}) =
      renderCon name <> renderObject renderInputField typeData <> defineTypeClass "INPUT_OBJECT"
    renderT (InputUnion _) = "\n -- Error: Input Union Not Supported"
    renderT (OutputObject DataTyCon {typeData}) =
      renderCon name <> renderObject (renderField context) typeData <> defineTypeClass "OBJECT"
    ----------------------------------------------------------------------------------------------------------
    typeIntro = "\n\n---- GQL " <> name <> " ------------------------------- \n"
    ----------------------------------------------------------------------------------------------------------
    defineTypeClass kind =
      "\n\n" <> renderTypeInstanceHead "GQLType" name <> indent <> "type KIND " <> name <> " = " <> kind <> "\n\n"
    ----------------------------------------------------------------------------------------------------------

renderTypeInstanceHead :: Text -> Text -> Text
renderTypeInstanceHead className name = "instance " <> className <> " " <> name <> " where\n"

renderGQLScalar :: Text -> Text
renderGQLScalar name = renderTypeInstanceHead "GQLScalar " name <> renderParse <> renderSerialize <> "\n\n"
  where
    renderParse = indent <> "parseValue _ = pure (" <> name <> " 0 0 )" <> "\n"
    renderSerialize = indent <> "serialize (" <> name <> " x y ) = Int (x + y)"

renderUnion :: Text -> [DataField] -> Text
renderUnion typeName = unionType . map renderElem
  where
    renderElem DataField {fieldType = TypeAlias {aliasTyCon}} = renderUnionCon typeName aliasTyCon <> aliasTyCon

unionType :: [Text] -> Text
unionType ls = "\n" <> indent <> intercalate ("\n" <> indent <> "| ") ls <> " deriving (Generic)"

renderObject :: (a -> (Text, Maybe Text)) -> [a] -> Text
renderObject f list = intercalate "\n\n" $ renderMainType : catMaybes types
  where
    renderMainType = renderSet fields <> " deriving (Generic)"
    (fields, types) = unzip (map f list)

renderInputField :: (Text, DataField) -> (Text, Maybe Text)
renderInputField (key, DataField {fieldType = TypeAlias {aliasTyCon, aliasWrappers}}) =
  (key `renderAssignment` renderWrapped aliasWrappers aliasTyCon, Nothing)

renderField :: Context -> (Text, DataField) -> (Text, Maybe Text)
renderField Context {scope, pubSub = (channel, content)} (key, DataField { fieldType = TypeAlias { aliasWrappers
                                                                                                 , aliasTyCon
                                                                                                 }
                                                                         , fieldArgs
                                                                         }) =
  (key `renderAssignment` argTypeName <> " -> " <> renderMonad scope <> result aliasWrappers, argTypes)
  where
    renderMonad Subscription = "IOSubRes " <> channel <> " " <> content <> " "
    renderMonad Mutation =
      case channel of
        "()" -> "IORes "
        _    -> "IOMutRes " <> channel <> " " <> content <> " "
    renderMonad _ = "IORes "
    -----------------------------------------------------------------
    result wrappers
      | isNullable wrappers = renderTuple (renderWrapped wrappers aliasTyCon)
      | otherwise = renderWrapped wrappers aliasTyCon
    (argTypeName, argTypes) = renderArguments fieldArgs
    renderArguments :: [(Text, DataArgument)] -> (Text, Maybe Text)
    renderArguments [] = ("()", Nothing)
    renderArguments list =
      ( fieldArgTypeName
      , Just (renderData fieldArgTypeName <> renderCon fieldArgTypeName <> renderObject renderInputField list))
      where
        fieldArgTypeName = "Arg" <> camelCase key
        camelCase :: Text -> Text
        camelCase ""   = ""
        camelCase text = toUpper (pack [T.head text]) <> T.tail text
