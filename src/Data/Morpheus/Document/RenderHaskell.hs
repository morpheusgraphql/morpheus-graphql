{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.RenderHaskell
  ( renderHaskellDocument
  ) where

import           Data.ByteString.Lazy.Char8        (ByteString)
import           Data.Maybe                        (catMaybes)
import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text, intercalate, toTitle)
import qualified Data.Text.Lazy                    as LT (fromStrict)
import           Data.Text.Lazy.Encoding           (encodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
                                                    DataType (..), DataTypeLib, allDataTypes, showWrappedType)

renderHaskellDocument :: DataTypeLib -> ByteString
renderHaskellDocument lib = encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map renderHaskellType visibleTypes
  where
    visibleTypes = allDataTypes lib

renderIndent :: Text
renderIndent = "  "

defineData :: Text -> Text
defineData name = "data " <> name <> " = " <> name <> " "

renderHaskellType :: (Text, DataFullType) -> Text
renderHaskellType (name, dataType) = defineData name <> renderType dataType
    ---
  where
    renderType (Leaf (LeafScalar _)) = ""
    renderType (Leaf (LeafEnum DataType {typeData})) = ""
    renderType (Union DataType {typeData}) = intercalate ("\n" <> renderIndent <> "| ") (map fieldType typeData)
    renderType (InputObject DataType {typeData}) = renderDataObject renderInputField typeData
    renderType (InputUnion DataType {typeData}) = renderDataObject renderInputField (mapKeys typeData)
    renderType (OutputObject DataType {typeData}) = renderDataObject renderField typeData

mapKeys :: [DataField a] -> [(Text, DataField a)]
mapKeys = map (\x -> (fieldName x, x))

renderObject :: (a -> (Text, Maybe Text)) -> [a] -> Text
renderObject f list = intercalate "\n\n" $ renderMainType : catMaybes types
  where
    renderMainType = "{ \n  " <> intercalate ("\n" <> renderIndent) fields <> "\n} deriving (Generic)"
    (fields, types) = unzip (map f list)

renderDataObject :: ((Text, DataField a) -> (Text, Maybe Text)) -> [(Text, DataField a)] -> Text
renderDataObject f list = renderObject f (ignoreHidden list)
  where
    ignoreHidden :: [(Text, DataField a)] -> [(Text, DataField a)]
    ignoreHidden = filter (not . fieldHidden . snd)

renderInputField :: (Text, DataField ()) -> (Text, Maybe Text)
renderInputField (key, DataField {fieldTypeWrappers, fieldType}) =
  (key <> " := " <> showWrappedType fieldTypeWrappers fieldType, Nothing)

renderField :: (Text, DataField [(Text, DataArgument)]) -> (Text, Maybe Text)
renderField (key, DataField {fieldTypeWrappers, fieldType, fieldArgs}) =
  (key <> " :: " <> argTypeName <> " -> ResM " <> showWrappedType fieldTypeWrappers fieldType, argTypes)
  where
    (argTypeName, argTypes) = renderArguments fieldArgs
    renderArguments :: [(Text, DataArgument)] -> (Text, Maybe Text)
    renderArguments [] = ("()", Nothing)
    renderArguments list =
      (fieldArgTypeName, Just (defineData fieldArgTypeName <> renderDataObject renderInputField list))
      where
        fieldArgTypeName = "Arg" <> toTitle key
