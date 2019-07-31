{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.RenderHaskell
  ( renderHaskellDocument
  ) where

import           Data.ByteString.Lazy.Char8        (ByteString)
import           Data.Maybe                        (catMaybes)
import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text, intercalate, toTitle, toUpper)
import qualified Data.Text.Lazy                    as LT (fromStrict)
import           Data.Text.Lazy.Encoding           (encodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
                                                    DataType (..), DataTypeLib, DataTypeWrapper (..), allDataTypes)

renderHaskellDocument :: DataTypeLib -> ByteString
renderHaskellDocument lib = encodeText $ renderLanguageExtensions <> renderImports <> types
  where
    encodeText = encodeUtf8 . LT.fromStrict
    types = intercalate "\n\n" $ map renderHaskellType visibleTypes
    visibleTypes = allDataTypes lib

renderIndent :: Text
renderIndent = "  "

defineData :: Text -> Text
defineData name = "data " <> name <> " = "

defineCon :: Text -> Text
defineCon name = name <> " "

typeAssignment :: Text -> Text -> Text
typeAssignment key value = key <> " :: " <> value

renderLanguageExtensions :: Text
renderLanguageExtensions = "{-# LANGUAGE DeriveGeneric #-}\n" <> "\n"

renderImports :: Text
renderImports = "import    Data.Morpheus.Types  (ResM)\n" <> "import    GHC.Generics  (Generic)\n" <> "\n"

renderHaskellType :: (Text, DataFullType) -> Text
renderHaskellType (name, dataType) = defineData name <> renderType dataType
  where
    renderType (Leaf (LeafScalar _)) = ""
    renderType (Leaf (LeafEnum DataType {typeData})) = renderEnum typeData
    renderType (Union DataType {typeData}) = renderUnion name typeData
    renderType (InputObject DataType {typeData}) = defineCon name <> renderDataObject renderInputField typeData
    renderType (InputUnion DataType {typeData}) = defineCon name <> renderDataObject renderInputField (mapKeys typeData)
    renderType (OutputObject DataType {typeData}) = defineCon name <> renderDataObject renderField typeData

mapKeys :: [DataField a] -> [(Text, DataField a)]
mapKeys = map (\x -> (fieldName x, x))

renderEnum :: [Text] -> Text
renderEnum elements = "\n  { " <> intercalate ("\n  ," <> renderIndent) elements <> "\n  } deriving (Generic)"

renderUnion :: Text -> [DataField ()] -> Text
renderUnion typeName unionNames =
  "\n" <> intercalate ("\n" <> renderIndent <> "| ") (map renderElem unionNames) <> "  deriving (Generic)"
  where
    renderElem DataField {fieldType} = defineCon (typeName <> "_" <> toUpper fieldType) <> fieldType

renderObject :: (a -> (Text, Maybe Text)) -> [a] -> Text
renderObject f list = intercalate "\n\n" $ renderMainType : catMaybes types
  where
    renderMainType = "\n  { " <> intercalate ("\n  ," <> renderIndent) fields <> "\n  } deriving (Generic)"
    (fields, types) = unzip (map f list)

renderDataObject :: ((Text, DataField a) -> (Text, Maybe Text)) -> [(Text, DataField a)] -> Text
renderDataObject f list = renderObject f (ignoreHidden list)
  where
    ignoreHidden :: [(Text, DataField a)] -> [(Text, DataField a)]
    ignoreHidden = filter (not . fieldHidden . snd)

renderMaybe :: Text -> Text
renderMaybe typeName = "Maybe " <> typeName

renderList :: Text -> Text
renderList typeName = "[" <> typeName <> "]"

renderWrappedType :: [DataTypeWrapper] -> Text -> Text
renderWrappedType [] typeName                          = renderMaybe typeName
renderWrappedType [NonNullType] typeName               = typeName
renderWrappedType (NonNullType:(ListType:xs)) typeName = renderList $ renderWrappedType xs typeName
renderWrappedType (ListType:xs) typeName               = renderMaybe $ renderList $ renderWrappedType xs typeName
renderWrappedType (NonNullType:xs) typeName            = renderWrappedType xs typeName

renderInputField :: (Text, DataField ()) -> (Text, Maybe Text)
renderInputField (key, DataField {fieldTypeWrappers, fieldType}) =
  (key `typeAssignment` renderWrappedType fieldTypeWrappers fieldType, Nothing)

renderField :: (Text, DataField [(Text, DataArgument)]) -> (Text, Maybe Text)
renderField (key, DataField {fieldTypeWrappers, fieldType, fieldArgs}) =
  (key `typeAssignment` argTypeName <> " -> ResM (" <> renderWrappedType fieldTypeWrappers fieldType <> ")", argTypes)
  where
    (argTypeName, argTypes) = renderArguments fieldArgs
    renderArguments :: [(Text, DataArgument)] -> (Text, Maybe Text)
    renderArguments [] = ("()", Nothing)
    renderArguments list =
      ( fieldArgTypeName
      , Just (defineData fieldArgTypeName <> defineCon fieldArgTypeName <> renderDataObject renderInputField list))
      where
        fieldArgTypeName = "Arg" <> toTitle key
