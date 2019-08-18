{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Rendering.GQL
  ( renderGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8        (ByteString)
import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text, intercalate)
import qualified Data.Text.Lazy                    as LT (fromStrict)
import           Data.Text.Lazy.Encoding           (encodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
                                                    DataType (..), DataTypeLib, allDataTypes, showWrappedType)

renderGraphQLDocument :: DataTypeLib -> ByteString
renderGraphQLDocument lib =
  encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map renderType visibleTypes
  where
    visibleTypes = filter (isVisible . snd) (allDataTypes lib)

isVisible :: DataFullType -> Bool
isVisible (Leaf (BaseScalar DataType {typeVisibility}))   = typeVisibility
isVisible (Leaf (CustomScalar DataType {typeVisibility})) = typeVisibility
isVisible (Leaf (LeafEnum DataType {typeVisibility}))     = typeVisibility
isVisible (Union DataType {typeVisibility})               = typeVisibility
isVisible (InputObject DataType {typeVisibility})         = typeVisibility
isVisible (InputUnion DataType {typeVisibility})          = typeVisibility
isVisible (OutputObject DataType {typeVisibility})        = typeVisibility

renderIndent :: Text
renderIndent = "  "

renderType :: (Text, DataFullType) -> Text
renderType (name, Leaf (BaseScalar _)) = "scalar " <> name
renderType (name, Leaf (CustomScalar _)) = "scalar " <> name
renderType (name, Leaf (LeafEnum DataType {typeData})) =
  "enum " <> name <> renderObject id typeData
renderType (name, Union DataType {typeData}) =
  "union " <> name <> " =\n    " <>
  intercalate ("\n" <> renderIndent <> "| ") (map fieldType typeData)
renderType (name, InputObject DataType {typeData}) =
  "input " <> name <> renderDataObject renderInputField typeData
renderType (name, InputUnion DataType {typeData}) =
  "input " <> name <> renderDataObject renderInputField (mapKeys typeData)
renderType (name, OutputObject DataType {typeData}) =
  "type " <> name <> renderDataObject renderField typeData

mapKeys :: [DataField a] -> [(Text, DataField a)]
mapKeys = map (\x -> (fieldName x, x))

renderObject :: (a -> Text) -> [a] -> Text
renderObject f list =
  " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n}"

renderDataObject ::
     ((Text, DataField a) -> Text) -> [(Text, DataField a)] -> Text
renderDataObject f list = renderObject f (ignoreHidden list)
  where
    ignoreHidden :: [(Text, DataField a)] -> [(Text, DataField a)]
    ignoreHidden = filter (not . fieldHidden . snd)

renderInputField :: (Text, DataField ()) -> Text
renderInputField (key, DataField {fieldTypeWrappers, fieldType}) =
  key <> ": " <> showWrappedType fieldTypeWrappers fieldType

renderField :: (Text, DataField [(Text, DataArgument)]) -> Text
renderField (key, DataField {fieldTypeWrappers, fieldType, fieldArgs}) =
  key <> renderArguments fieldArgs <> ": " <>
  showWrappedType fieldTypeWrappers fieldType
  where
    renderArguments :: [(Text, DataArgument)] -> Text
    renderArguments [] = ""
    renderArguments list =
      "(" <> intercalate ", " (map renderInputField list) <> ")"
