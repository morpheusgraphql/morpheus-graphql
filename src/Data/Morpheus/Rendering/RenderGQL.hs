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
import           Data.Morpheus.Types.Internal.Data  (DataField (..), DataTyCon (..), DataType (..), DataTypeLib,
                                                     DataTypeWrapper (..), Key, TypeAlias (..), TypeWrapper (..),
                                                     allDataTypes, createInputUnionFields, fieldVisibility,
                                                     isDefaultTypeName, toGQLWrapper)
import           Data.Morpheus.Types.Internal.Value (convertToJSONName)

renderGraphQLDocument :: DataTypeLib -> ByteString
renderGraphQLDocument lib = encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map render visibleTypes
  where
    visibleTypes = filter (not . isDefaultTypeName . fst) (allDataTypes lib)

class RenderGQL a where
  render :: a -> Key
  renderWrapped :: a -> [TypeWrapper] -> Key
  default renderWrapped :: a -> [TypeWrapper] -> Key
  renderWrapped x wrappers = showGQLWrapper (toGQLWrapper wrappers)
    where
      showGQLWrapper []               = render x
      showGQLWrapper (ListType:xs)    = "[" <> showGQLWrapper xs <> "]"
      showGQLWrapper (NonNullType:xs) = showGQLWrapper xs <> "!"

instance RenderGQL Key where
  render = id

instance RenderGQL TypeAlias where
  render TypeAlias {aliasTyCon, aliasWrappers} = renderWrapped aliasTyCon aliasWrappers

instance RenderGQL DataType where
  render (DataScalar x)      = typeName x
  render (DataEnum x)        = typeName x
  render (DataUnion x)       = typeName x
  render (DataInputObject x) = typeName x
  render (DataInputUnion x)  = typeName x
  render ( DataObject x)     = typeName x

instance RenderGQL (Key, DataType) where
  render (name, DataScalar {}) = "scalar " <> name
  render (name, DataEnum DataTyCon {typeData}) = "enum " <> name <> renderObject id typeData
  render (name, DataUnion DataTyCon {typeData}) =
    "union " <> name <> " =\n    " <> intercalate ("\n" <> renderIndent <> "| ") typeData
  render (name, DataInputObject DataTyCon {typeData}) = "input " <> name <> render typeData
  render (name, DataInputUnion DataTyCon {typeData}) = "input " <> name <> render fields
    where
     fields = createInputUnionFields name typeData
  render (name, DataObject DataTyCon {typeData}) = "type " <> name <> render typeData



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
      ignoreHidden = filter fieldVisibility

renderIndent :: Text
renderIndent = "  "

renderObject :: (a -> Text) -> [a] -> Text
renderObject f list = " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n}"
