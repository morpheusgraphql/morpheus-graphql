{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.RenderHaskell
  ( renderHaskellDocument
  ) where

import           Data.ByteString.Lazy.Char8        (ByteString)
import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text, intercalate)
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

renderHaskellType :: (Text, DataFullType) -> Text
renderHaskellType (name, dataType) = defineData <> renderType dataType
  where
    defineData = "data " <> name <> " = " <> name
    ---
    renderType (Leaf (LeafScalar _)) = ""
    renderType (Leaf (LeafEnum DataType {typeData})) = renderObject id typeData
    renderType (Union DataType {typeData}) = intercalate ("\n" <> renderIndent <> "| ") (map fieldType typeData)
    renderType (InputObject DataType {typeData}) = renderDataObject renderInputField typeData
    renderType (InputUnion DataType {typeData}) = renderDataObject renderInputField (mapKeys typeData)
    renderType (OutputObject DataType {typeData}) = renderDataObject renderField typeData

mapKeys :: [DataField a] -> [(Text, DataField a)]
mapKeys = map (\x -> (fieldName x, x))

renderObject :: (a -> Text) -> [a] -> Text
renderObject f list = " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n} deriving (Generic)"

renderDataObject :: ((Text, DataField a) -> Text) -> [(Text, DataField a)] -> Text
renderDataObject f list = renderObject f (ignoreHidden list)
  where
    ignoreHidden :: [(Text, DataField a)] -> [(Text, DataField a)]
    ignoreHidden = filter (not . fieldHidden . snd)

renderInputField :: (Text, DataField ()) -> Text
renderInputField (key, DataField {fieldTypeWrappers, fieldType}) =
  key <> " := " <> showWrappedType fieldTypeWrappers fieldType

renderField :: (Text, DataField [(Text, DataArgument)]) -> Text
renderField (key, DataField {fieldTypeWrappers, fieldType, fieldArgs}) =
  key <> " :: " <> renderArguments fieldArgs <> " -> ResM " <> showWrappedType fieldTypeWrappers fieldType
  where
    renderArguments :: [(Text, DataArgument)] -> Text
    renderArguments []   = "()"
    renderArguments list = "(" <> intercalate ", " (map renderInputField list) <> ")"
