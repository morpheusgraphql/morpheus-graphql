{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Rendering.RenderIntrospection
  ( render
  , createObjectType
  ) where

import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text, unpack)

import           Data.Morpheus.Schema.Schema

-- Morpheus
import           Data.Morpheus.Schema.TypeKind      (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data  (DataField (..), DataField, DataFullType (..), DataLeaf (..),
                                                     DataObject, DataTyCon (..), DataTypeKind (..), DataTypeLib,
                                                     DataTypeWrapper (..), DataUnion, TypeAlias (..), kindOf,
                                                     lookupDataType, toGQLWrapper)
import           Data.Morpheus.Types.Internal.Value (convertToJSONName)

constRes :: Applicative m => a -> b -> m a
constRes = const . pure

type Result a = DataTypeLib -> Either String a

class RenderSchema a b where
  render :: Monad m => (Text, a) -> Result (b m)

instance RenderSchema DataFullType S__Type where
  render (name, Leaf leaf) = render (name, leaf)
  render (name, InputObject iObject) = renderInputObject (name, iObject)
  render (name, OutputObject object') = typeFromObject (name, object')
    where
      typeFromObject (key, DataTyCon {typeData, typeDescription}) lib =
        createObjectType key typeDescription <$>
        (Just <$> traverse (`render` lib) (filter (not . fieldHidden . snd) typeData))
  render (name, Union union') = const $ pure $ typeFromUnion (name, union')
  render (name, InputUnion inpUnion') = renderInputUnion (name, inpUnion')

instance RenderSchema DataLeaf S__Type where
  render (key, BaseScalar DataTyCon {typeDescription}) _ = pure $ createLeafType SCALAR key typeDescription Nothing
  render (key, CustomScalar DataTyCon {typeDescription}) _ = pure $ createLeafType SCALAR key typeDescription Nothing
  render (key, LeafEnum DataTyCon {typeDescription, typeData}) _ =
    pure $ createLeafType ENUM key typeDescription (Just $ map createEnumValue typeData)

instance RenderSchema DataField S__Field where
  render (key, field@DataField {fieldType = TypeAlias {aliasTyCon}, fieldArgs}) lib = do
    kind <- renderTypeKind <$> lookupKind aliasTyCon lib
    createFieldWith key (wrap field $ createType kind aliasTyCon Nothing $ Just []) <$>
      traverse (`inputValueFromArg` lib) fieldArgs

renderTypeKind :: DataTypeKind -> TypeKind
renderTypeKind KindScalar      = SCALAR
renderTypeKind (KindObject _)  = OBJECT
renderTypeKind KindUnion       = UNION
renderTypeKind KindInputUnion  = INPUT_OBJECT
renderTypeKind KindEnum        = ENUM
renderTypeKind KindInputObject = INPUT_OBJECT
renderTypeKind KindList        = LIST
renderTypeKind KindNonNull     = NON_NULL

wrap :: Applicative m => DataField -> S__Type m -> S__Type m
wrap DataField {fieldType = TypeAlias {aliasWrappers}} typ = foldr wrapByTypeWrapper typ (toGQLWrapper aliasWrappers)

wrapByTypeWrapper :: Applicative m => DataTypeWrapper -> S__Type m -> S__Type m
wrapByTypeWrapper ListType    = wrapAs LIST
wrapByTypeWrapper NonNullType = wrapAs NON_NULL

lookupKind :: Text -> Result DataTypeKind
lookupKind name lib =
  case lookupDataType name lib of
    Nothing    -> Left $ unpack $ "Kind Not Found: " <> name
    Just value -> Right (kindOf value)

inputValueFromArg :: Applicative m => (Text, DataField) -> Result (S__InputValue m)
inputValueFromArg (key, input) = fmap (createInputValueWith key) . createInputObjectType input

createInputObjectType :: Applicative m => DataField -> Result (S__Type m)
createInputObjectType field@DataField {fieldType = TypeAlias {aliasTyCon}} lib = do
  kind <- renderTypeKind <$> lookupKind aliasTyCon lib
  pure $ wrap field $ createType kind aliasTyCon Nothing $ Just []

renderInputObject :: Applicative m => (Text, DataObject) -> Result (S__Type m)
renderInputObject (key, DataTyCon {typeData, typeDescription}) lib = do
  fields <- traverse (`inputValueFromArg` lib) typeData
  pure $ createInputObject key typeDescription fields

renderInputUnion :: Applicative m => (Text, DataUnion) -> Result (S__Type m)
renderInputUnion (key', DataTyCon {typeData, typeDescription}) lib =
  createInputObject key' typeDescription <$> traverse createField typeData
  where
    createField field = createInputValueWith (fieldName field) <$> createInputObjectType field lib

createLeafType :: Applicative m => TypeKind -> Text -> Maybe Text -> Maybe [S__EnumValue m] -> S__Type m
createLeafType kind name description enums =
  S__Type
    { s__TypeKind = constRes kind
    , s__TypeName = constRes $ Just name
    , s__TypeDescription = constRes description
    , s__TypeFields = constRes Nothing
    , s__TypeOfType = constRes Nothing
    , s__TypeInterfaces = constRes Nothing
    , s__TypePossibleTypes = constRes Nothing
    , s__TypeEnumValues = constRes enums
    , s__TypeInputFields = constRes Nothing
    }

typeFromUnion :: Applicative m => (Text, DataUnion) -> S__Type m
typeFromUnion (name, DataTyCon {typeData, typeDescription}) =
  S__Type
    { s__TypeKind = constRes UNION
    , s__TypeName = constRes $ Just name
    , s__TypeDescription = constRes typeDescription
    , s__TypeFields = constRes Nothing
    , s__TypeOfType = constRes Nothing
    , s__TypeInterfaces = constRes Nothing
    , s__TypePossibleTypes =
        constRes $ Just (map (\x -> createObjectType (aliasTyCon $ fieldType x) Nothing $ Just []) typeData)
    , s__TypeEnumValues = constRes Nothing
    , s__TypeInputFields = constRes Nothing
    }

createObjectType :: Applicative m => Text -> Maybe Text -> Maybe [S__Field m] -> S__Type m
createObjectType name description fields =
  S__Type
    { s__TypeKind = constRes OBJECT
    , s__TypeName = constRes $ Just name
    , s__TypeDescription = constRes description
    , s__TypeFields = constRes fields
    , s__TypeOfType = constRes Nothing
    , s__TypeInterfaces = constRes $ Just []
    , s__TypePossibleTypes = constRes Nothing
    , s__TypeEnumValues = constRes Nothing
    , s__TypeInputFields = constRes Nothing
    }

createInputObject :: Applicative m => Text -> Maybe Text -> [S__InputValue m] -> S__Type m
createInputObject name description fields =
  S__Type
    { s__TypeKind = constRes INPUT_OBJECT
    , s__TypeName = constRes $ Just name
    , s__TypeDescription = constRes description
    , s__TypeFields = constRes Nothing
    , s__TypeOfType = constRes Nothing
    , s__TypeInterfaces = constRes Nothing
    , s__TypePossibleTypes = constRes Nothing
    , s__TypeEnumValues = constRes Nothing
    , s__TypeInputFields = constRes $ Just fields
    }

createType :: Applicative m => TypeKind -> Text -> Maybe Text -> Maybe [S__Field m] -> S__Type m
createType kind name description fields' =
  S__Type
    { s__TypeKind = constRes kind
    , s__TypeName = constRes $ Just name
    , s__TypeDescription = constRes description
    , s__TypeFields = constRes fields'
    , s__TypeOfType = constRes Nothing
    , s__TypeInterfaces = constRes Nothing
    , s__TypePossibleTypes = constRes Nothing
    , s__TypeEnumValues = constRes $ Just []
    , s__TypeInputFields = constRes Nothing
    }

wrapAs :: Applicative m => TypeKind -> S__Type m -> S__Type m
wrapAs kind contentType =
  S__Type
    { s__TypeKind = constRes kind
    , s__TypeName = constRes Nothing
    , s__TypeDescription = constRes Nothing
    , s__TypeFields = constRes Nothing
    , s__TypeOfType = constRes $ Just contentType
    , s__TypeInterfaces = constRes Nothing
    , s__TypePossibleTypes = constRes Nothing
    , s__TypeEnumValues = constRes Nothing
    , s__TypeInputFields = constRes Nothing
    }

createFieldWith :: Applicative m => Text -> S__Type m -> [S__InputValue m] -> S__Field m
createFieldWith _name fieldType fieldArgs =
  S__Field
    { s__FieldName = constRes $ convertToJSONName _name
    , s__FieldDescription = constRes Nothing
    , s__FieldArgs = constRes fieldArgs
    , s__FieldType' = constRes fieldType
    , s__FieldIsDeprecated = constRes False
    , s__FieldDeprecationReason = constRes Nothing
    }

createInputValueWith :: Applicative m => Text -> S__Type m -> S__InputValue m
createInputValueWith name ivType =
  S__InputValue
    { s__InputValueName = constRes name
    , s__InputValueDescription = constRes Nothing
    , s__InputValueType' = constRes ivType
    , s__InputValueDefaultValue = constRes Nothing
    }

createEnumValue :: Applicative m => Text -> S__EnumValue m
createEnumValue name =
  S__EnumValue
    { s__EnumValueName = constRes name
    , s__EnumValueDescription = constRes Nothing
    , s__EnumValueIsDeprecated = constRes False
    , s__EnumValueDeprecationReason = constRes Nothing
    }
