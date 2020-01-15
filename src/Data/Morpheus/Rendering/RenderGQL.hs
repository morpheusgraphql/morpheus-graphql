{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE GADTs                #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL(..)
  , renderGraphQLDocument
  , renderWrapped
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
                                                ( FieldDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
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
                                                , ArgumentsDefinition(..)
                                                , Name
                                                , FieldsDefinition(..)
                                                , Listable(..)
                                                , Named(..)
                                                )

renderGraphQLDocument :: Schema -> ByteString
renderGraphQLDocument lib =
  encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map render visibleTypes
 where
  visibleTypes = filter (not . isDefaultTypeName . typeName) (allDataTypes lib)

class RenderGQL a where
  render :: a -> Key

instance RenderGQL TypeDefinition where
  render TypeDefinition { typeName, typeContent } = __render typeContent
   where
    __render DataInterface { interfaceFields } = "interface " <> typeName <> render interfaceFields
    __render DataScalar{}    = "scalar " <> typeName
    __render (DataEnum tags) = "enum " <> typeName <> renderObject render tags
    __render (DataUnion members) =
      "union "
        <> typeName
        <> " =\n    "
        <> intercalate ("\n" <> renderIndent <> "| ") members
    __render (DataInputObject fields ) = "input " <> typeName <> render fields
    __render (DataInputUnion  members) = "input " <> typeName <> render (fromList fields :: FieldsDefinition )
      where fields = createInputUnionFields typeName (fmap fst members)
    __render DataObject {objectFields} = "type " <> typeName <> render objectFields

-- OBJECT
instance RenderGQL FieldsDefinition where
  render = renderObject render . ignoreHidden . toList
   where 
    ignoreHidden :: [FieldDefinition] -> [FieldDefinition]
    ignoreHidden = filter fieldVisibility

instance RenderGQL FieldDefinition where 
  render FieldDefinition { fieldName, fieldType, fieldArgs } =
    convertToJSONName fieldName <> render fieldArgs <> ": " <> render fieldType

instance RenderGQL ArgumentsDefinition where 
  render NoArguments   = ""
  render ArgumentsDefinition { arguments } = "(" <> intercalate ", " (map unName $ toList $ fmap render arguments) <> ")"

instance RenderGQL DataEnumValue where
  render DataEnumValue { enumName } = enumName

instance RenderGQL TypeRef where
  render TypeRef { typeConName, typeWrappers } = renderWrapped typeConName typeWrappers

instance RenderGQL Key where
  render = id

renderWrapped :: RenderGQL a => a -> [TypeWrapper] -> Name
renderWrapped x wrappers = showGQLWrapper (toGQLWrapper wrappers)
    where
      showGQLWrapper []               = render x
      showGQLWrapper (ListType:xs)    = "[" <> showGQLWrapper xs <> "]"
      showGQLWrapper (NonNullType:xs) = showGQLWrapper xs <> "!"

renderIndent :: Text
renderIndent = "  "

renderObject :: (a -> Text) -> [a] -> Text
renderObject f list =
  " { \n  " <> intercalate ("\n" <> renderIndent) (fmap f list) <> "\n}"