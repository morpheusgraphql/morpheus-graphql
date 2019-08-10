{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Internal.RenderIntrospection
  ( Type
  , Field
  , InputValue
  , renderType
  , createObjectType
  ) where

import           Data.Morpheus.Schema.EnumValue    (EnumValue, createEnumValue)
import qualified Data.Morpheus.Schema.Field        as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue   as IN (InputValue (..), createInputValueWith)
import           Data.Morpheus.Schema.Type         (Type (..))
import           Data.Morpheus.Schema.TypeKind     (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data (DataField (..), DataFullType (..), DataInputField, DataInputObject,
                                                    DataLeaf (..), DataOutputField, DataOutputObject, DataType (..),
                                                    DataTypeKind (..), DataTypeWrapper (..), DataUnion)
import           Data.Text                         (Text)

renderType :: (Text, DataFullType) -> Type
renderType (name', Leaf leaf')           = typeFromLeaf (name', leaf')
renderType (name', InputObject iObject') = renderInputObject (name', iObject')
renderType (name', OutputObject object') = typeFromObject (name', object')
renderType (name', Union union')         = typeFromUnion (name', union')
renderType (name', InputUnion inpUnion') = renderInputUnion (name', inpUnion')

type InputValue = IN.InputValue Type

type Field = F.Field Type

renderTypeKind :: DataTypeKind -> TypeKind
renderTypeKind KindScalar      = SCALAR
renderTypeKind KindObject      = OBJECT
renderTypeKind KindUnion       = UNION
renderTypeKind KindInputUnion  = OBJECT
renderTypeKind KindEnum        = ENUM
renderTypeKind KindInputObject = INPUT_OBJECT
renderTypeKind KindList        = LIST
renderTypeKind KindNonNull     = NON_NULL

wrap :: DataField a -> Type -> Type
wrap field' = wrapRec (fieldTypeWrappers field')

wrapRec :: [DataTypeWrapper] -> Type -> Type
wrapRec xs type' = foldr wrapByTypeWrapper type' xs

wrapByTypeWrapper :: DataTypeWrapper -> Type -> Type
wrapByTypeWrapper ListType    = wrapAs LIST
wrapByTypeWrapper NonNullType = wrapAs NON_NULL

fieldFromObjectField :: (Text, DataOutputField) -> Field
fieldFromObjectField (key', field'@DataField {fieldType = type', fieldKind = kind', fieldArgs = args'}) =
  F.createFieldWith
    key'
    (wrap field' $ createType (renderTypeKind kind') type' "" $ Just [])
    (map inputValueFromArg args')

typeFromLeaf :: (Text, DataLeaf) -> Type
typeFromLeaf (key', LeafScalar DataType {typeDescription = desc'}) = createLeafType SCALAR key' desc' Nothing
typeFromLeaf (key', LeafEnum DataType {typeDescription = desc', typeData = tags'}) =
  createLeafType ENUM key' desc' (Just $ map createEnumValue tags')

createLeafType :: TypeKind -> Text -> Text -> Maybe [EnumValue] -> Type
createLeafType kind' name' desc' enums' =
  Type
    { kind = kind'
    , name = Just name'
    , description = Just desc'
    , fields = const $ return Nothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = const $ return enums'
    , inputFields = Nothing
    }

typeFromUnion :: (Text, DataUnion) -> Type
typeFromUnion (name', DataType {typeData = fields', typeDescription = description'}) =
  Type
    { kind = UNION
    , name = Just name'
    , description = Just description'
    , fields = const $ return Nothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Just (map (\x -> createObjectType (fieldType x) "" $ Just []) fields')
    , enumValues = const $ return Nothing
    , inputFields = Nothing
    }

typeFromObject :: (Text, DataOutputObject) -> Type
typeFromObject (key', DataType {typeData = fields', typeDescription = description'}) =
  createObjectType key' description' (Just $ map fieldFromObjectField $ filter (not . fieldHidden . snd) fields')

inputValueFromArg :: (Text, DataInputField) -> InputValue
inputValueFromArg (key', input') = IN.createInputValueWith key' (createInputObjectType input')

createInputObjectType :: DataInputField -> Type
createInputObjectType field' =
  wrap field' $ createType (renderTypeKind $ fieldKind field') (fieldType field') "" $ Just []

renderInputObject :: (Text, DataInputObject) -> Type
renderInputObject (key', DataType {typeData = fields', typeDescription = description'}) =
  createInputObject key' description' (map inputValueFromArg fields')

renderInputUnion :: (Text, DataUnion) -> Type
renderInputUnion (key', DataType {typeData = fields', typeDescription = description'}) =
  createInputObject key' description' (map createField fields')
  where
    createField field = IN.createInputValueWith (fieldName field) (createInputObjectType field)

createObjectType :: Text -> Text -> Maybe [Field] -> Type
createObjectType name' desc' fields' =
  Type
    { kind = OBJECT
    , name = Just name'
    , description = Just desc'
    , fields = const $ return fields'
    , ofType = Nothing
    , interfaces = Just []
    , possibleTypes = Nothing
    , enumValues = const $ return Nothing
    , inputFields = Nothing
    }

createInputObject :: Text -> Text -> [InputValue] -> Type
createInputObject name' desc' fields' =
  Type
    { kind = INPUT_OBJECT
    , name = Just name'
    , description = Just desc'
    , fields = const $ return Nothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = const $ return Nothing
    , inputFields = Just fields'
    }

createType :: TypeKind -> Text -> Text -> Maybe [Field] -> Type
createType kind' name' desc' fields' =
  Type
    { kind = kind'
    , name = Just name'
    , description = Just desc'
    , fields = const $ return fields'
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = const $ return $ Just []
    , inputFields = Nothing
    }

wrapAs :: TypeKind -> Type -> Type
wrapAs kind' contentType =
  Type
    { kind = kind'
    , name = Nothing
    , description = Nothing
    , fields = const $ return Nothing
    , ofType = Just contentType
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = const $ return Nothing
    , inputFields = Nothing
    }
