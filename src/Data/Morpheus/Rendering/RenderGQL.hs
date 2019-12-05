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
                                                , DataTypeLib
                                                , DataTypeWrapper(..)
                                                , Key
                                                , TypeAlias(..)
                                                , TypeWrapper(..)
                                                , allDataTypes
                                                , createInputUnionFields
                                                , fieldVisibility
                                                , isDefaultTypeName
                                                , toGQLWrapper
                                                , DataEnumValue(..)
                                                , convertToJSONName
                                                )


renderGraphQLDocument :: DataTypeLib -> ByteString
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

instance RenderGQL TypeAlias where
  render TypeAlias { aliasTyCon, aliasWrappers } =
    renderWrapped aliasTyCon aliasWrappers

instance RenderGQL DataType where
  render = typeName

instance RenderGQL DataEnumValue where
  render DataEnumValue { enumName } = enumName

instance RenderGQL (Key, DataType) where
  render (name, DataType { typeContent }) = __render typeContent
   where
    __render DataScalar{} = "scalar " <> name
    __render (DataEnum typeData) =
      "enum " <> name <> renderObject render typeData
    __render (DataUnion typeData) =
      "union "
        <> name
        <> " =\n    "
        <> intercalate ("\n" <> renderIndent <> "| ") typeData
    __render (DataInputObject typeData) = "input " <> name <> render typeData
    __render (DataInputUnion  typeData) = "input " <> name <> render fields
      where fields = createInputUnionFields name (map fst typeData)
    __render (DataObject typeData) = "type " <> name <> render typeData

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
