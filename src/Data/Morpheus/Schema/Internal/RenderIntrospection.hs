{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Internal.RenderIntrospection
  ( Type
  , Field
  , InputValue
  , createObjectType
  , typeFromObject
  , typeFromInputObject
  , typeFromLeaf
  , typeFromUnion
  ) where

import           Data.Morpheus.Schema.EnumValue    (EnumValue, createEnumValue)
import qualified Data.Morpheus.Schema.Field        as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue   as IN (InputValue (..), createInputValueWith)
import           Data.Morpheus.Schema.Type         (Type (..))
import           Data.Morpheus.Schema.TypeKind     (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data (DataField (..), DataInputField, DataInputObject, DataLeaf (..),
                                                    DataOutputField, DataOutputObject, DataType (..),
                                                    DataTypeWrapper (..), DataUnion)
import           Data.Morpheus.Types.Resolver      ((::->))
import           Data.Text                         (Text)

type InputValue = IN.InputValue Type

type Field = F.Field Type

inputValueFromArg :: (Text, DataInputField) -> InputValue
inputValueFromArg (key', input') = IN.createInputValueWith key' (createInputObjectType input')

createInputObjectType :: DataInputField -> Type
createInputObjectType field' = wrap field' $ createType (fieldKind field') (fieldType field') "" []

wrap :: DataField a -> Type -> Type
wrap field' = wrapRec (fieldTypeWrappers field')

wrapRec :: [DataTypeWrapper] -> Type -> Type
wrapRec [] type'     = type'
wrapRec (x:xs) type' = wrapByTypeWrapper x (wrapRec xs type')

wrapByTypeWrapper :: DataTypeWrapper -> Type -> Type
wrapByTypeWrapper ListType    = wrapAs LIST
wrapByTypeWrapper NonNullType = wrapAs NON_NULL

fieldFromObjectField :: (Text, DataOutputField) -> Field
fieldFromObjectField (key', field'@DataField {fieldType = type', fieldKind = kind', fieldArgs = args'}) =
  F.createFieldWith key' (wrap field' $ createType kind' type' "" []) (map inputValueFromArg args')

typeFromLeaf :: (Text, DataLeaf) -> Type
typeFromLeaf (key', LeafScalar DataType {typeDescription = desc'}) = createLeafType SCALAR key' desc' []
typeFromLeaf (key', LeafEnum DataType {typeDescription = desc', typeData = tags'}) =
  createLeafType ENUM key' desc' (map createEnumValue tags')

resolveNothing :: a ::-> Maybe b
resolveNothing = return Nothing

resolveMaybeList :: [b] -> a ::-> Maybe [b]
resolveMaybeList list' = return (Just list')

createLeafType :: TypeKind -> Text -> Text -> [EnumValue] -> Type
createLeafType kind' name' desc' enums' =
  Type
    { kind = kind'
    , name = Just name'
    , description = Just desc'
    , fields = resolveNothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveMaybeList enums'
    , inputFields = Nothing
    }

typeFromUnion :: (Text, DataUnion) -> Type
typeFromUnion (name', fields') =
  Type
    { kind = UNION
    , name = Just name'
    , description = Just "TODO"
    , fields = resolveNothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Just (map (\x -> createObjectType (fieldType x) "" []) fields')
    , enumValues = return Nothing
    , inputFields = Nothing
    }

typeFromObject :: (Text, DataOutputObject) -> Type
typeFromObject (key', DataType {typeData = fields', typeDescription = description'}) =
  createObjectType key' description' (map fieldFromObjectField fields')

typeFromInputObject :: (Text, DataInputObject) -> Type
typeFromInputObject (key', DataType {typeData = fields', typeDescription = description'}) =
  createInputObject key' description' (map inputValueFromArg fields')

createObjectType :: Text -> Text -> [Field] -> Type
createObjectType = createType OBJECT

createInputObject :: Text -> Text -> [InputValue] -> Type
createInputObject name' desc' fields' =
  Type
    { kind = INPUT_OBJECT
    , name = Just name'
    , description = Just desc'
    , fields = resolveMaybeList []
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveNothing
    , inputFields = Just fields'
    }

createType :: TypeKind -> Text -> Text -> [Field] -> Type
createType kind' name' desc' fields' =
  Type
    { kind = kind'
    , name = Just name'
    , description = Just desc'
    , fields = resolveMaybeList fields'
    , ofType = Nothing
    , interfaces = Just []
    , possibleTypes = Just []
    , enumValues = resolveMaybeList []
    , inputFields = Just []
    }

wrapAs :: TypeKind -> Type -> Type
wrapAs kind' contentType =
  Type
    { kind = kind'
    , name = Nothing
    , description = Nothing
    , fields = resolveNothing
    , ofType = Just contentType
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveNothing
    , inputFields = Nothing
    }
