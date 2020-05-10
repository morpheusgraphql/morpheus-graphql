{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
where

-- MORPHEUS
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    DataEnumValue (..),
    DataTypeWrapper (..),
    FieldDefinition (..),
    FieldsDefinition (..),
    InputFieldsDefinition (..),
    Key,
    Name,
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    TypeWrapper (..),
    convertToJSONName,
    createInputUnionFields,
    fieldVisibility,
    isDefaultTypeName,
    toGQLWrapper,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Listable (..),
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    intercalate,
  )

class RenderGQL a where
  render :: a -> Key

instance RenderGQL Schema where
  render schema = intercalate "\n\n" $ map render visibleTypes
    where
      visibleTypes = filter (not . isDefaultTypeName . typeName) (toList schema)

instance RenderGQL (TypeDefinition a) where
  render TypeDefinition {typeName, typeContent} = __render typeContent
    where
      __render DataInterface {interfaceFields} = "interface " <> typeName <> render interfaceFields
      __render DataScalar {} = "scalar " <> typeName
      __render (DataEnum tags) = "enum " <> typeName <> renderObject render tags
      __render (DataUnion members) =
        "union "
          <> typeName
          <> " =\n    "
          <> intercalate ("\n" <> renderIndent <> "| ") members
      __render (DataInputObject fields) = "input " <> typeName <> render fields
      __render (DataInputUnion members) = "input " <> typeName <> render fieldsDef
        where
          fieldsDef = unsafeFromFields fields
          fields = createInputUnionFields typeName (fmap fst members)
      __render DataObject {objectFields} = "type " <> typeName <> render objectFields

ignoreHidden :: [FieldDefinition] -> [FieldDefinition]
ignoreHidden = filter fieldVisibility

-- OBJECT
instance RenderGQL FieldsDefinition where
  render = renderObject render . ignoreHidden . toList

instance RenderGQL InputFieldsDefinition where
  render = renderObject render . ignoreHidden . toList

instance RenderGQL FieldDefinition where
  render FieldDefinition {fieldName, fieldType, fieldArgs} =
    convertToJSONName fieldName <> render fieldArgs <> ": " <> render fieldType

instance RenderGQL ArgumentsDefinition where
  render NoArguments = ""
  render arguments = "(" <> intercalate ", " (map render $ toList arguments) <> ")"

instance RenderGQL DataEnumValue where
  render DataEnumValue {enumName} = enumName

instance RenderGQL TypeRef where
  render TypeRef {typeConName, typeWrappers} = renderWrapped typeConName typeWrappers

instance RenderGQL Key where
  render = id

renderWrapped :: RenderGQL a => a -> [TypeWrapper] -> Name
renderWrapped x wrappers = showGQLWrapper (toGQLWrapper wrappers)
  where
    showGQLWrapper [] = render x
    showGQLWrapper (ListType : xs) = "[" <> showGQLWrapper xs <> "]"
    showGQLWrapper (NonNullType : xs) = showGQLWrapper xs <> "!"

renderIndent :: Text
renderIndent = "  "

renderObject :: (a -> Text) -> [a] -> Text
renderObject f list =
  " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n}"
