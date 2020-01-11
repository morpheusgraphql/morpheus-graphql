{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts  , FlexibleInstances    #-}

module Data.Morpheus.Rendering.RenderIntrospection
  ( render
  , createObjectType
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Data.Maybe                     ( isJust )


-- Morpheus
import           Data.Morpheus.Schema.Schema
import           Data.Morpheus.Schema.TypeKind  ( TypeKind(..) )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataInputUnion
                                                , FieldDefinition(..)
                                                , DataTypeContent(..)
                                                , DataType(..)
                                                , DataTypeKind(..)
                                                , Schema
                                                , DataTypeWrapper(..)
                                                , DataUnion
                                                , Meta(..)
                                                , TypeRef(..)
                                                , createInputUnionFields
                                                , fieldVisibility
                                                , kindOf
                                                , lookupDataType
                                                , toGQLWrapper
                                                , DataEnumValue(..)
                                                , lookupDeprecated
                                                , DataInputUnion
                                                , lookupDeprecatedReason
                                                , convertToJSONName
                                                , ArgumentsDefinition(..)
                                                , Listable(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Failure(..) )

constRes :: Applicative m => a -> b -> m a
constRes = const . pure

type Result m a = Schema -> m a

class RenderSchema a b where
  render :: (Monad m, Failure Text m) => a -> Schema -> m (b m)

instance RenderSchema DataType S__Type where
  render DataType { typeName , typeMeta, typeContent } = __render typeContent
   where
    __render
      :: (Monad m, Failure Text m) => DataTypeContent -> Schema -> m (S__Type m)
    __render DataScalar{} =
      constRes $ createLeafType SCALAR typeName typeMeta Nothing
    __render (DataEnum enums) = constRes
      $ createLeafType ENUM typeName typeMeta (Just $ map createEnumValue enums)
    __render (DataInputObject fields) = \lib ->
      createInputObject typeName typeMeta
        <$> traverse (`renderinputValue` lib) (toList fields)
    __render (DataObject {objectFields}) = \lib ->
      createObjectType typeName (typeMeta >>= metaDescription)
        <$> (Just <$> traverse (`render` lib) (filter fieldVisibility $ toList objectFields))
    __render (DataUnion union) =
      constRes $ typeFromUnion (typeName, typeMeta, union)
    __render (DataInputUnion members) =
      renderInputUnion (typeName, typeMeta, members)

createEnumValue :: Monad m => DataEnumValue -> S__EnumValue m
createEnumValue DataEnumValue { enumName, enumMeta } = S__EnumValue
  { s__EnumValueName              = pure enumName
  , s__EnumValueDescription       = pure (enumMeta >>= metaDescription)
  , s__EnumValueIsDeprecated      = pure (isJust deprecated)
  , s__EnumValueDeprecationReason = pure (deprecated >>= lookupDeprecatedReason)
  }
  where deprecated = enumMeta >>= lookupDeprecated

renderArguments :: (Monad m, Failure Text m) => ArgumentsDefinition -> Schema -> m [S__InputValue m] 
renderArguments ArgumentsDefinition { arguments} lib = traverse (`renderinputValue` lib) arguments
renderArguments NoArguments _ = pure []

instance RenderSchema FieldDefinition S__Field where
  render field@FieldDefinition { fieldName ,fieldType = TypeRef { typeConName }, fieldArgs, fieldMeta } lib
    = do
      kind <- renderTypeKind <$> lookupKind typeConName lib
      pure S__Field
        { s__FieldName              = pure (convertToJSONName fieldName)
        , s__FieldDescription       = pure (fieldMeta >>= metaDescription)
        , s__FieldArgs              = renderArguments fieldArgs lib 
        , s__FieldType'             =
          pure (applyTypeWrapper field $ createType kind typeConName Nothing $ Just [])
        , s__FieldIsDeprecated      = pure (isJust deprecated)
        , s__FieldDeprecationReason = pure
                                        (deprecated >>= lookupDeprecatedReason)
        }
    where deprecated = fieldMeta >>= lookupDeprecated

renderTypeKind :: DataTypeKind -> TypeKind
renderTypeKind KindScalar      = SCALAR
renderTypeKind (KindObject _)  = OBJECT
renderTypeKind KindUnion       = UNION
renderTypeKind KindInputUnion  = INPUT_OBJECT
renderTypeKind KindEnum        = ENUM
renderTypeKind KindInputObject = INPUT_OBJECT
renderTypeKind KindList        = LIST
renderTypeKind KindNonNull     = NON_NULL

applyTypeWrapper :: Monad m => FieldDefinition -> S__Type m -> S__Type m
applyTypeWrapper FieldDefinition { fieldType = TypeRef { typeWrappers } } typ =
  foldr wrapByTypeWrapper typ (toGQLWrapper typeWrappers)

wrapByTypeWrapper :: Monad m => DataTypeWrapper -> S__Type m -> S__Type m
wrapByTypeWrapper ListType    = wrapAs LIST
wrapByTypeWrapper NonNullType = wrapAs NON_NULL

lookupKind :: (Monad m, Failure Text m) => Text -> Result m DataTypeKind
lookupKind name lib = case lookupDataType name lib of
  Nothing    -> failure $ "Kind Not Found: " <> name
  Just value -> pure (kindOf value)

renderinputValue
  :: (Monad m, Failure Text m)
  => FieldDefinition
  -> Result m (S__InputValue m)
renderinputValue input = fmap (createInputValueWith (fieldName input) (fieldMeta input)) . createInputObjectType input

createInputObjectType
  :: (Monad m, Failure Text m) => FieldDefinition -> Result m (S__Type m)
createInputObjectType field@FieldDefinition { fieldType = TypeRef { typeConName } } lib
  = do
    kind <- renderTypeKind <$> lookupKind typeConName lib
    pure $ applyTypeWrapper field $ createType kind typeConName Nothing $ Just []


renderInputUnion
  :: (Monad m, Failure Text m)
  => (Text, Maybe Meta, DataInputUnion)
  -> Result m (S__Type m)
renderInputUnion (key, meta, fields) lib =
  createInputObject key meta <$> traverse
    createField
    (createInputUnionFields key $ map fst $ filter snd fields)
 where
  createField field =
    createInputValueWith (fieldName field) Nothing <$> createInputObjectType field lib

createLeafType
  :: Monad m
  => TypeKind
  -> Text
  -> Maybe Meta
  -> Maybe [S__EnumValue m]
  -> S__Type m
createLeafType kind name meta enums = S__Type
  { s__TypeKind          = pure kind
  , s__TypeName          = pure $ Just name
  , s__TypeDescription   = pure (meta >>= metaDescription)
  , s__TypeFields        = constRes Nothing
  , s__TypeOfType        = pure Nothing
  , s__TypeInterfaces    = pure Nothing
  , s__TypePossibleTypes = pure Nothing
  , s__TypeEnumValues    = constRes enums
  , s__TypeInputFields   = pure Nothing
  }

typeFromUnion :: Monad m => (Text, Maybe Meta, DataUnion) -> S__Type m
typeFromUnion (name, typeMeta, typeContent) = S__Type
  { s__TypeKind          = pure UNION
  , s__TypeName          = pure $ Just name
  , s__TypeDescription   = pure (typeMeta >>= metaDescription)
  , s__TypeFields        = constRes Nothing
  , s__TypeOfType        = pure Nothing
  , s__TypeInterfaces    = pure Nothing
  , s__TypePossibleTypes =
    pure $ Just (map (\x -> createObjectType x Nothing $ Just []) typeContent)
  , s__TypeEnumValues    = constRes Nothing
  , s__TypeInputFields   = pure Nothing
  }

createObjectType
  :: Monad m => Text -> Maybe Text -> Maybe [S__Field m] -> S__Type m
createObjectType name description fields = S__Type
  { s__TypeKind          = pure OBJECT
  , s__TypeName          = pure $ Just name
  , s__TypeDescription   = pure description
  , s__TypeFields        = constRes fields
  , s__TypeOfType        = pure Nothing
  , s__TypeInterfaces    = pure $ Just []
  , s__TypePossibleTypes = pure Nothing
  , s__TypeEnumValues    = constRes Nothing
  , s__TypeInputFields   = pure Nothing
  }

createInputObject
  :: Monad m => Text -> Maybe Meta -> [S__InputValue m] -> S__Type m
createInputObject name meta fields = S__Type
  { s__TypeKind          = pure INPUT_OBJECT
  , s__TypeName          = pure $ Just name
  , s__TypeDescription   = pure (meta >>= metaDescription)
  , s__TypeFields        = constRes Nothing
  , s__TypeOfType        = pure Nothing
  , s__TypeInterfaces    = pure Nothing
  , s__TypePossibleTypes = pure Nothing
  , s__TypeEnumValues    = constRes Nothing
  , s__TypeInputFields   = pure $ Just fields
  }

createType
  :: Monad m
  => TypeKind
  -> Text
  -> Maybe Text
  -> Maybe [S__Field m]
  -> S__Type m
createType kind name description fields = S__Type
  { s__TypeKind          = pure kind
  , s__TypeName          = pure $ Just name
  , s__TypeDescription   = pure description
  , s__TypeFields        = constRes fields
  , s__TypeOfType        = pure Nothing
  , s__TypeInterfaces    = pure Nothing
  , s__TypePossibleTypes = pure Nothing
  , s__TypeEnumValues    = constRes $ Just []
  , s__TypeInputFields   = pure Nothing
  }

wrapAs :: Monad m => TypeKind -> S__Type m -> S__Type m
wrapAs kind contentType = S__Type { s__TypeKind          = pure kind
                                  , s__TypeName          = pure Nothing
                                  , s__TypeDescription   = pure Nothing
                                  , s__TypeFields        = constRes Nothing
                                  , s__TypeOfType = pure $ Just contentType
                                  , s__TypeInterfaces    = pure Nothing
                                  , s__TypePossibleTypes = pure Nothing
                                  , s__TypeEnumValues    = constRes Nothing
                                  , s__TypeInputFields   = pure Nothing
                                  }

createInputValueWith
  :: Monad m => Text -> Maybe Meta -> S__Type m -> S__InputValue m
createInputValueWith name meta ivType = S__InputValue
  { s__InputValueName         = pure (convertToJSONName name)
  , s__InputValueDescription  = pure (meta >>= metaDescription)
  , s__InputValueType'        = pure ivType
  , s__InputValueDefaultValue = pure Nothing
  }
