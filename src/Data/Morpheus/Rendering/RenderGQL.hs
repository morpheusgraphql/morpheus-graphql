{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL(..)
  , renderGraphQLDocument
  )
where

import           Data.ByteString.Lazy.Char8     ( ByteString )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , intercalate
                                                )
import qualified Data.Text.Lazy                as LT
                                                ( fromStrict )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST
                                                ( DataField(..)
                                                , DataTypeContent(..)
                                                , DataType(..)
                                                , Schema
                                                , DataTypeWrapper(..)
                                                , Key
                                                , TypeRef(..)
                                                , TypeWrapper(..)
                                                , allDataTypes
                                                , createInputUnionFields
                                                , fieldVisibility
                                                , isDefaultTypeName
                                                , toGQLWrapper
                                                , DataEnumValue(..)
                                                , convertToJSONName
                                                )


renderGraphQLDocument :: Schema -> ByteString
renderGraphQLDocument lib =
  encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map render visibleTypes
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

instance RenderGQL TypeRef where
  render TypeRef { typeConName, typeWrappers } =
    renderWrapped typeConName typeWrappers

instance RenderGQL DataType where
  render = typeName

instance RenderGQL DataEnumValue where
  render DataEnumValue { enumName } = enumName

instance RenderGQL (Key, DataType) where
  render (name, DataType { typeContent }) = __render typeContent
   where
    __render DataInterface { interfaceFields } = "interface " <> name <> render interfaceFields
    __render DataScalar{}    = "scalar " <> name
    __render (DataEnum tags) = "enum " <> name <> renderObject render tags
    __render (DataUnion members) =
      "union "
        <> name
        <> " =\n    "
        <> intercalate ("\n" <> renderIndent <> "| ") members
    __render (DataInputObject fields ) = "input " <> name <> render fields
    __render (DataInputUnion  members) = "input " <> name <> render fields
      where fields = createInputUnionFields name (map fst members)
    __render (DataObject {objectFields}) = "type " <> name <> render objectFields

-- OBJECT
instance RenderGQL [(Text, DataField)] where
  render = renderObject renderField . ignoreHidden
   where
    renderField :: (Text, DataField) -> Text
    renderField (key, DataField { fieldType, fieldArgs }) =
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
renderObject f list =
  " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n}"
