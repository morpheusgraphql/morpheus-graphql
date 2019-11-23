{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}

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
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataField(..)
                                                , DataTyCon(..)
                                                , DataType(..)
                                                , DataTypeKind(..)
                                                , DataTypeLib
                                                , DataTypeWrapper(..)
                                                , DataUnion
                                                , Meta(..)
                                                , TypeAlias(..)
                                                , createInputUnionFields
                                                , fieldVisibility
                                                , kindOf
                                                , lookupDataType
                                                , toGQLWrapper
                                                , DataEnumValue(..)
                                                , lookupDeprecated
                                                , lookupDeprecatedReason
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( convertToJSONName )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Failure(..) )

constRes :: Applicative m => a -> b -> m a
constRes = const . pure

type Result m a = DataTypeLib -> m a

class RenderSchema a b where
  render :: (Monad m, Failure Text m) => (Text, a) -> DataTypeLib -> m (b m)

instance RenderSchema DataType S__Type where
  render (key, DataScalar DataTyCon { typeMeta }) =
    constRes $ createLeafType SCALAR key typeMeta Nothing
  render (key, DataEnum DataTyCon { typeMeta, typeData }) =
    constRes
      $ createLeafType ENUM key typeMeta (Just $ map createEnumValue typeData)
  render (name, DataInputObject DataTyCon { typeData, typeMeta }) =
    renderInputObject
   where
    renderInputObject lib = do
      fields <- traverse (`renderinputValue` lib) typeData
      pure $ createInputObject name typeMeta fields
  render (name, DataObject object') = typeFromObject (name, object')
   where
    typeFromObject (key, DataTyCon { typeData, typeMeta }) lib =
      createObjectType key (typeMeta >>= metaDescription)
        <$> (Just <$> traverse (`render` lib) (filter fieldVisibility typeData))
  render (name, DataUnion union) = constRes $ typeFromUnion (name, union)
  render (name, DataInputUnion inpUnion') = renderInputUnion (name, inpUnion')

createEnumValue :: Monad m => DataEnumValue -> S__EnumValue m
createEnumValue DataEnumValue { enumName, enumMeta } = S__EnumValue
  { s__EnumValueName              = constRes enumName
  , s__EnumValueDescription       = constRes (enumMeta >>= metaDescription)
  , s__EnumValueIsDeprecated      = constRes (isJust deprecated)
  , s__EnumValueDeprecationReason = constRes
                                      (deprecated >>= lookupDeprecatedReason)
  }
  where deprecated = enumMeta >>= lookupDeprecated

instance RenderSchema DataField S__Field where
  render (name, field@DataField { fieldType = TypeAlias { aliasTyCon }, fieldArgs, fieldMeta }) lib
    = do
      kind <- renderTypeKind <$> lookupKind aliasTyCon lib
      args <- traverse (`renderinputValue` lib) fieldArgs
      pure S__Field
        { s__FieldName              = constRes (convertToJSONName name)
        , s__FieldDescription       = constRes (fieldMeta >>= metaDescription)
        , s__FieldArgs              = constRes args
        , s__FieldType'             =
          constRes (wrap field $ createType kind aliasTyCon Nothing $ Just [])
        , s__FieldIsDeprecated      = constRes (isJust deprecated)
        , s__FieldDeprecationReason = constRes
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

wrap :: Monad m => DataField -> S__Type m -> S__Type m
wrap DataField { fieldType = TypeAlias { aliasWrappers } } typ =
  foldr wrapByTypeWrapper typ (toGQLWrapper aliasWrappers)

wrapByTypeWrapper :: Monad m => DataTypeWrapper -> S__Type m -> S__Type m
wrapByTypeWrapper ListType    = wrapAs LIST
wrapByTypeWrapper NonNullType = wrapAs NON_NULL

lookupKind :: (Monad m, Failure Text m) => Text -> Result m DataTypeKind
lookupKind name lib = case lookupDataType name lib of
  Nothing    -> failure $ "Kind Not Found: " <> name
  Just value -> pure (kindOf value)

renderinputValue
  :: (Monad m, Failure Text m)
  => (Text, DataField)
  -> Result m (S__InputValue m)
renderinputValue (key, input) =
  fmap (createInputValueWith key (fieldMeta input))
    . createInputObjectType input

createInputObjectType
  :: (Monad m, Failure Text m) => DataField -> Result m (S__Type m)
createInputObjectType field@DataField { fieldType = TypeAlias { aliasTyCon } } lib
  = do
    kind <- renderTypeKind <$> lookupKind aliasTyCon lib
    pure $ wrap field $ createType kind aliasTyCon Nothing $ Just []


renderInputUnion
  :: (Monad m, Failure Text m) => (Text, DataUnion) -> Result m (S__Type m)
renderInputUnion (key, DataTyCon { typeData, typeMeta }) lib =
  createInputObject key typeMeta
    <$> traverse createField (createInputUnionFields key typeData)
 where
  createField (name, field) =
    createInputValueWith name Nothing <$> createInputObjectType field lib

createLeafType
  :: Monad m
  => TypeKind
  -> Text
  -> Maybe Meta
  -> Maybe [S__EnumValue m]
  -> S__Type m
createLeafType kind name meta enums = S__Type
  { s__TypeKind          = constRes kind
  , s__TypeName          = constRes $ Just name
  , s__TypeDescription   = constRes (meta >>= metaDescription)
  , s__TypeFields        = constRes Nothing
  , s__TypeOfType        = constRes Nothing
  , s__TypeInterfaces    = constRes Nothing
  , s__TypePossibleTypes = constRes Nothing
  , s__TypeEnumValues    = constRes enums
  , s__TypeInputFields   = constRes Nothing
  }

typeFromUnion :: Monad m => (Text, DataUnion) -> S__Type m
typeFromUnion (name, DataTyCon { typeData, typeMeta }) = S__Type
  { s__TypeKind          = constRes UNION
  , s__TypeName          = constRes $ Just name
  , s__TypeDescription   = constRes (typeMeta >>= metaDescription)
  , s__TypeFields        = constRes Nothing
  , s__TypeOfType        = constRes Nothing
  , s__TypeInterfaces    = constRes Nothing
  , s__TypePossibleTypes =
    constRes $ Just (map (\x -> createObjectType x Nothing $ Just []) typeData)
  , s__TypeEnumValues    = constRes Nothing
  , s__TypeInputFields   = constRes Nothing
  }

createObjectType
  :: Monad m => Text -> Maybe Text -> Maybe [S__Field m] -> S__Type m
createObjectType name description fields = S__Type
  { s__TypeKind          = constRes OBJECT
  , s__TypeName          = constRes $ Just name
  , s__TypeDescription   = constRes description
  , s__TypeFields        = constRes fields
  , s__TypeOfType        = constRes Nothing
  , s__TypeInterfaces    = constRes $ Just []
  , s__TypePossibleTypes = constRes Nothing
  , s__TypeEnumValues    = constRes Nothing
  , s__TypeInputFields   = constRes Nothing
  }

createInputObject
  :: Monad m => Text -> Maybe Meta -> [S__InputValue m] -> S__Type m
createInputObject name meta fields = S__Type
  { s__TypeKind          = constRes INPUT_OBJECT
  , s__TypeName          = constRes $ Just name
  , s__TypeDescription   = constRes (meta >>= metaDescription)
  , s__TypeFields        = constRes Nothing
  , s__TypeOfType        = constRes Nothing
  , s__TypeInterfaces    = constRes Nothing
  , s__TypePossibleTypes = constRes Nothing
  , s__TypeEnumValues    = constRes Nothing
  , s__TypeInputFields   = constRes $ Just fields
  }

createType
  :: Monad m
  => TypeKind
  -> Text
  -> Maybe Text
  -> Maybe [S__Field m]
  -> S__Type m
createType kind name description fields = S__Type
  { s__TypeKind          = constRes kind
  , s__TypeName          = constRes $ Just name
  , s__TypeDescription   = constRes description
  , s__TypeFields        = constRes fields
  , s__TypeOfType        = constRes Nothing
  , s__TypeInterfaces    = constRes Nothing
  , s__TypePossibleTypes = constRes Nothing
  , s__TypeEnumValues    = constRes $ Just []
  , s__TypeInputFields   = constRes Nothing
  }

wrapAs :: Monad m => TypeKind -> S__Type m -> S__Type m
wrapAs kind contentType = S__Type { s__TypeKind          = constRes kind
                                  , s__TypeName          = constRes Nothing
                                  , s__TypeDescription   = constRes Nothing
                                  , s__TypeFields        = constRes Nothing
                                  , s__TypeOfType = constRes $ Just contentType
                                  , s__TypeInterfaces    = constRes Nothing
                                  , s__TypePossibleTypes = constRes Nothing
                                  , s__TypeEnumValues    = constRes Nothing
                                  , s__TypeInputFields   = constRes Nothing
                                  }

createInputValueWith
  :: Monad m => Text -> Maybe Meta -> S__Type m -> S__InputValue m
createInputValueWith name meta ivType = S__InputValue
  { s__InputValueName         = constRes (convertToJSONName name)
  , s__InputValueDescription  = constRes (meta >>= metaDescription)
  , s__InputValueType'        = constRes ivType
  , s__InputValueDefaultValue = constRes Nothing
  }
