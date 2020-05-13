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
    Name (..),
    Schema,
    Token,
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
  ( intercalate,
  )

class RenderGQL a where
  render :: a -> Token

instance RenderGQL Schema where
  render schema = intercalate "\n\n" $ map render visibleTypes
    where
      visibleTypes = filter (not . isDefaultTypeName . typeName) (toList schema)

instance RenderGQL (TypeDefinition a) where
  render TypeDefinition {typeName, typeContent} = __render typeContent
    where
      __render DataInterface {interfaceFields} = "interface " <> render typeName <> render interfaceFields
      __render DataScalar {} = "scalar " <> render typeName
      __render (DataEnum tags) = "enum " <> render typeName <> renderObject render tags
      __render (DataUnion members) =
        "union "
          <> render typeName
          <> " =\n    "
          <> intercalate ("\n" <> renderIndent <> "| ") (map render members)
      __render (DataInputObject fields) = "input " <> render typeName <> render fields
      __render (DataInputUnion members) = "input " <> render typeName <> render fieldsDef
        where
          fieldsDef = unsafeFromFields fields
          fields = createInputUnionFields typeName (fmap fst members)
      __render DataObject {objectFields} = "type " <> render typeName <> render objectFields

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
  render DataEnumValue {enumName} = render enumName

instance RenderGQL TypeRef where
  render TypeRef {typeConName, typeWrappers} = renderWrapped typeConName typeWrappers

instance RenderGQL Name where
  render = readName

renderWrapped :: RenderGQL a => a -> [TypeWrapper] -> Token
renderWrapped x wrappers = showGQLWrapper (toGQLWrapper wrappers)
  where
    showGQLWrapper [] = render x
    showGQLWrapper (ListType : xs) = "[" <> showGQLWrapper xs <> "]"
    showGQLWrapper (NonNullType : xs) = showGQLWrapper xs <> "!"

renderIndent :: Token
renderIndent = "  "

renderObject :: (a -> Token) -> [a] -> Token
renderObject f list =
  " { \n  " <> intercalate ("\n" <> renderIndent) (map f list) <> "\n}"
