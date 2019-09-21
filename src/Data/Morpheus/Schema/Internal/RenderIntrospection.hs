{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Internal.RenderIntrospection
  ( Type
  , Field
  , InputValue
  , renderType
  , createObjectType
  ) where

import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text, unpack)

-- Morpheus
import           Data.Morpheus.Schema.EnumValue    (EnumValue, createEnumValue)
import qualified Data.Morpheus.Schema.Field        as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue   as IN (InputValue (..), createInputValueWith)
import           Data.Morpheus.Schema.Type         (Type (..))
import           Data.Morpheus.Schema.TypeKind     (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data (DataField (..), DataField, DataFullType (..), DataLeaf (..),
                                                    DataObject, DataTyCon (..), DataTypeKind (..), DataTypeLib,
                                                    DataTypeWrapper (..), DataUnion, WrapperD, kindOf, lookupDataType,
                                                    toGQLWrapper)

type InputValue = IN.InputValue Type

type Result a = DataTypeLib -> Either String a

type Field = F.Field Type

renderType :: (Text, DataFullType) -> Result Type
renderType (name', Leaf leaf') = const $ pure $ typeFromLeaf (name', leaf')
renderType (name, InputObject iObject) = renderInputObject (name, iObject)
renderType (name', OutputObject object') = typeFromObject (name', object')
  where
    typeFromObject (key, DataTyCon {typeData, typeDescription}) lib =
      createObjectType key typeDescription <$>
      (Just <$> traverse (`fieldFromObjectField` lib) (filter (not . fieldHidden . snd) typeData))
renderType (name', Union union') = const $ pure $ typeFromUnion (name', union')
renderType (name', InputUnion inpUnion') = renderInputUnion (name', inpUnion')

renderTypeKind :: DataTypeKind -> TypeKind
renderTypeKind KindScalar      = SCALAR
renderTypeKind KindObject      = OBJECT
renderTypeKind KindUnion       = UNION
renderTypeKind KindInputUnion  = INPUT_OBJECT
renderTypeKind KindEnum        = ENUM
renderTypeKind KindInputObject = INPUT_OBJECT
renderTypeKind KindList        = LIST
renderTypeKind KindNonNull     = NON_NULL

wrap :: DataField -> Type -> Type
wrap field' = wrapRec (fieldTypeWrappers field')

wrapRec :: [WrapperD] -> Type -> Type
wrapRec xs typ = foldr wrapByTypeWrapper typ (toGQLWrapper xs)

wrapByTypeWrapper :: DataTypeWrapper -> Type -> Type
wrapByTypeWrapper ListType    = wrapAs LIST
wrapByTypeWrapper NonNullType = wrapAs NON_NULL

lookupKind :: Text -> Result DataTypeKind
lookupKind name lib =
  case lookupDataType name lib of
    Nothing    -> Left $ unpack $ "Kind Not Found: " <> name
    Just value -> Right (kindOf value)

fieldFromObjectField :: (Text, DataField) -> Result Field
fieldFromObjectField (key, field'@DataField {fieldType, fieldArgs}) lib = do
  kind <- renderTypeKind <$> lookupKind fieldType lib
  F.createFieldWith key (wrap field' $ createType kind fieldType "" $ Just []) <$>
    traverse (`inputValueFromArg` lib) fieldArgs

typeFromLeaf :: (Text, DataLeaf) -> Type
typeFromLeaf (key, BaseScalar DataTyCon {typeDescription}) = createLeafType SCALAR key typeDescription Nothing
typeFromLeaf (key, CustomScalar DataTyCon {typeDescription}) = createLeafType SCALAR key typeDescription Nothing
typeFromLeaf (key, LeafEnum DataTyCon {typeDescription, typeData}) =
  createLeafType ENUM key typeDescription (Just $ map createEnumValue typeData)

createLeafType :: TypeKind -> Text -> Maybe Text -> Maybe [EnumValue] -> Type
createLeafType kind' name' description enums' =
  Type
    { kind = kind'
    , name = Just name'
    , description
    , fields = const $ return Nothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = const $ return enums'
    , inputFields = Nothing
    }

typeFromUnion :: (Text, DataUnion) -> Type
typeFromUnion (name', DataTyCon {typeData = fields', typeDescription = description}) =
  Type
    { kind = UNION
    , name = Just name'
    , description
    , fields = const $ return Nothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Just (map (\x -> createObjectType (fieldType x) Nothing $ Just []) fields')
    , enumValues = const $ return Nothing
    , inputFields = Nothing
    }

inputValueFromArg :: (Text, DataField) -> Result InputValue
inputValueFromArg (key, input) = fmap (IN.createInputValueWith key) . createInputObjectType input

createInputObjectType :: DataField -> Result Type
createInputObjectType field@DataField {fieldType} lib = do
  kind <- renderTypeKind <$> lookupKind fieldType lib
  pure $ wrap field $ createType kind fieldType "" $ Just []

renderInputObject :: (Text, DataObject) -> Result Type
renderInputObject (key, DataTyCon {typeData, typeDescription}) lib = do
  fields <- traverse (`inputValueFromArg` lib) typeData
  pure $ createInputObject key typeDescription fields

renderInputUnion :: (Text, DataUnion) -> Result Type
renderInputUnion (key', DataTyCon {typeData, typeDescription}) lib =
  createInputObject key' typeDescription <$> traverse createField typeData
  where
    createField field = IN.createInputValueWith (fieldName field) <$> createInputObjectType field lib

createObjectType :: Text -> Maybe Text -> Maybe [Field] -> Type
createObjectType name' description fields' =
  Type
    { kind = OBJECT
    , name = Just name'
    , description
    , fields = const $ return fields'
    , ofType = Nothing
    , interfaces = Just []
    , possibleTypes = Nothing
    , enumValues = const $ return Nothing
    , inputFields = Nothing
    }

createInputObject :: Text -> Maybe Text -> [InputValue] -> Type
createInputObject name' description fields' =
  Type
    { kind = INPUT_OBJECT
    , name = Just name'
    , description
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
