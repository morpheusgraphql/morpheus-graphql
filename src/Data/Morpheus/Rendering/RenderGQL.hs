{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL(..)
  , renderGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8         (ByteString)
import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text, intercalate)
import qualified Data.Text.Lazy                     as LT (fromStrict)
import           Data.Text.Lazy.Encoding            (encodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data  (DataField (..), DataFullType (..), DataKind (..), DataLeaf (..),
                                                     DataTyCon (..), DataTypeLib, DataTypeWrapper (..), Key,
                                                     TypeAlias (..), WrapperD (..), allDataTypes, isDefaultTypeName,
                                                     toGQLWrapper)
import           Data.Morpheus.Types.Internal.Value (convertToJSONName)

renderGraphQLDocument :: DataTypeLib -> ByteString
renderGraphQLDocument lib = encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map render visibleTypes
  where
    visibleTypes = filter (not . isDefaultTypeName . fst) (allDataTypes lib)

class RenderGQL a where
  render :: a -> Key
  renderWrapped :: a -> [WrapperD] -> Key
  default renderWrapped :: a -> [WrapperD] -> Key
  renderWrapped x wrappers = showGQLWrapper (toGQLWrapper wrappers)
    where
      showGQLWrapper []               = render x
      showGQLWrapper (ListType:xs)    = "[" <> showGQLWrapper xs <> "]"
      showGQLWrapper (NonNullType:xs) = showGQLWrapper xs <> "!"

instance RenderGQL Key where
  render = id

instance RenderGQL TypeAlias where
  render TypeAlias {aliasTyCon, aliasWrappers} = renderWrapped aliasTyCon aliasWrappers

instance RenderGQL DataKind where
  render (ScalarKind x) = typeName x
  render (EnumKind x)   = typeName x
  render (ObjectKind x) = typeName x
  render (UnionKind x)  = typeName x

instance RenderGQL (Key, DataFullType) where
  render (name, Leaf (DataScalar _)) = "scalar " <> name
  render (name, Leaf (DataEnum DataTyCon {typeData})) = "enum " <> name <> renderObject id typeData
  render (name, Union DataTyCon {typeData}) =
    "union " <> name <> " =\n    " <> intercalate ("\n" <> renderIndent <> "| ") (map (render . fieldType) typeData)
  render (name, InputObject DataTyCon {typeData}) = "input " <> name <> render typeData
  render (name, InputUnion DataTyCon {typeData}) = "input " <> name <> render (mapKeys typeData)
  render (name, OutputObject DataTyCon {typeData}) = "type " <> name <> render typeData

mapKeys :: [DataField] -> [(Text, DataField)]
mapKeys = map (\x -> (fieldName x, x))

-- OBJECT
instance RenderGQL [(Text, DataField)] where
  render = renderObject renderField . ignoreHidden
    where
      renderField :: (Text, DataField) -> Text
      renderField (key, DataField {fieldType, fieldArgs}) =
        convertToJSONName key <> renderArgs fieldArgs <> ": " <> render fieldType
        where
          renderArgs []   = ""
          renderArgs list = "(" <> intercalate ", " (map renderField list) <> ")"
      -----------------------------------------------------------
      ignoreHidden :: [(Text, DataField)] -> [(Text, DataField)]
      ignoreHidden = filter (not . fieldHidden . snd)

renderIndent :: Text
renderIndent = "  "

renderObject :: (a -> Text) -> [a] -> Text
renderObject f list = " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n}"
