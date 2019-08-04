{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Rendering.Types
  ( renderType
  ) where

import           Data.Maybe                             (catMaybes)
import           Data.Semigroup                         ((<>))
import           Data.Text                              (Text, intercalate, pack, toUpper)
import qualified Data.Text                              as T (head, tail)

-- MORPHEUS
import           Data.Morpheus.Document.Rendering.Terms (Context (..), Scope (..), indent, renderAssignment, renderCon,
                                                         renderData, renderSet, renderTuple, renderUnionCon,
                                                         renderWrapped)
import           Data.Morpheus.Types.Internal.Data      (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
                                                         DataType (..), DataTypeWrapper (..))

renderType :: Context -> (Text, DataFullType) -> Text
renderType context (name, dataType) = typeIntro <> renderData name <> renderT dataType
  where
    renderT (Leaf (LeafScalar _)) = renderCon name <> "Int Int" <> defineTypeClass "SCALAR" <> renderGQLScalar name
    renderT (Leaf (LeafEnum DataType {typeData})) = unionType typeData <> defineTypeClass "ENUM"
    renderT (Union DataType {typeData}) = renderUnion name typeData <> defineTypeClass "UNION"
    renderT (InputObject DataType {typeData}) =
      renderCon name <> renderObject renderInputField typeData <> defineTypeClass "INPUT_OBJECT"
    renderT (InputUnion _) = "\n -- Error: Input Union Not Supported"
    renderT (OutputObject DataType {typeData}) =
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

renderUnion :: Text -> [DataField ()] -> Text
renderUnion typeName = unionType . map renderElem
  where
    renderElem DataField {fieldType} = renderUnionCon typeName fieldType <> fieldType

unionType :: [Text] -> Text
unionType ls = "\n" <> indent <> intercalate ("\n" <> indent <> "| ") ls <> " deriving (Generic)"

renderObject :: (a -> (Text, Maybe Text)) -> [a] -> Text
renderObject f list = intercalate "\n\n" $ renderMainType : catMaybes types
  where
    renderMainType = renderSet fields <> " deriving (Generic)"
    (fields, types) = unzip (map f list)

renderInputField :: (Text, DataField ()) -> (Text, Maybe Text)
renderInputField (key, DataField {fieldTypeWrappers, fieldType}) =
  (key `renderAssignment` renderWrapped fieldTypeWrappers fieldType, Nothing)

renderField :: Context -> (Text, DataField [(Text, DataArgument)]) -> (Text, Maybe Text)
renderField Context {scope, pubSub = (channel, content)} (key, DataField {fieldTypeWrappers, fieldType, fieldArgs}) =
  (key `renderAssignment` argTypeName <> " -> " <> renderMonad scope <> result fieldTypeWrappers, argTypes)
  where
    renderMonad Subscription = "IOSubRes " <> channel <> " " <> content <> " "
    renderMonad Mutation =
      case channel of
        "()" -> "IORes "
        _    -> "IOMutRes " <> channel <> " " <> content <> " "
    renderMonad _ = "IORes "
    -----------------------------------------------------------------
    result wrappers@(NonNullType:_) = renderWrapped wrappers fieldType
    result wrappers                 = renderTuple (renderWrapped wrappers fieldType)
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
