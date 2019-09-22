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
import       Data.Morpheus.Rendering.RenderGQL (RenderGQL(..))
import       Data.Morpheus.Types.Internal.Data (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
                                                    DataTyCon (..), DataTypeLib, allDataTypes)

renderGraphQLDocument :: DataTypeLib -> ByteString
renderGraphQLDocument lib = encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map renderType visibleTypes
  where
    visibleTypes = filter (isVisible . snd) (allDataTypes lib)

isVisible :: DataFullType -> Bool
isVisible (Leaf (BaseScalar DataTyCon {typeVisibility}))   = typeVisibility
isVisible (Leaf (CustomScalar DataTyCon {typeVisibility})) = typeVisibility
isVisible (Leaf (LeafEnum DataTyCon {typeVisibility}))     = typeVisibility
isVisible (Union DataTyCon {typeVisibility})               = typeVisibility
isVisible (InputObject DataTyCon {typeVisibility})         = typeVisibility
isVisible (InputUnion DataTyCon {typeVisibility})          = typeVisibility
isVisible (OutputObject DataTyCon {typeVisibility})        = typeVisibility

renderIndent :: Text
renderIndent = "  "

renderType :: (Text, DataFullType) -> Text
renderType (name, Leaf (BaseScalar _)) = "scalar " <> name
renderType (name, Leaf (CustomScalar _)) = "scalar " <> name
renderType (name, Leaf (LeafEnum DataTyCon {typeData})) = "enum " <> name <> renderObject id typeData
renderType (name, Union DataTyCon {typeData}) =
  "union " <> name <> " =\n    " <> intercalate ("\n" <> renderIndent <> "| ") (map (render . fieldType) typeData)
renderType (name, InputObject DataTyCon {typeData}) = "input " <> name <> renderDataObject renderInputField typeData
renderType (name, InputUnion DataTyCon {typeData}) =
  "input " <> name <> renderDataObject renderInputField (mapKeys typeData)
renderType (name, OutputObject DataTyCon {typeData}) = "type " <> name <> renderDataObject renderField typeData

mapKeys :: [DataField] -> [(Text, DataField)]
mapKeys = map (\x -> (fieldName x, x))

renderObject :: (a -> Text) -> [a] -> Text
renderObject f list = " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n}"

renderDataObject :: ((Text, DataField) -> Text) -> [(Text, DataField)] -> Text
renderDataObject f list = renderObject f (ignoreHidden list)
  where
    ignoreHidden :: [(Text, DataField)] -> [(Text, DataField)]
    ignoreHidden = filter (not . fieldHidden . snd)

renderInputField :: (Text, DataField) -> Text
renderInputField (key, DataField {fieldType}) =
  key <> ": " <> render fieldType

renderField :: (Text, DataField) -> Text
renderField (key, DataField {fieldType, fieldArgs}) =
  key <> renderArguments fieldArgs <> ": " <> render fieldType
  where
    renderArguments :: [(Text, DataArgument)] -> Text
    renderArguments []   = ""
    renderArguments list = "(" <> intercalate ", " (map renderInputField list) <> ")"
