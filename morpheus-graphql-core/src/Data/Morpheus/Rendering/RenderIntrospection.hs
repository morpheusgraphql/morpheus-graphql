{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Rendering.RenderIntrospection
  ( render,
    createObjectType,
  )
where

import Data.Maybe (isJust)
-- Morpheus

import Data.Morpheus.Internal.Utils
  ( elems,
    failure,
    selectBy,
  )
import Data.Morpheus.Schema.TypeKind (TypeKind (..))
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    DataEnumValue (..),
    DataInputUnion,
    DataInputUnion,
    DataTypeKind (..),
    DataTypeWrapper (..),
    DataUnion,
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    Message,
    Meta (..),
    OUT,
    QUERY,
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    convertToJSONName,
    createInputUnionFields,
    fieldVisibility,
    kindOf,
    lookupDeprecated,
    lookupDeprecatedReason,
    msg,
    toGQLWrapper,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResModel,
    Resolver,
    mkBoolean,
    mkList,
    mkNull,
    mkObject,
    mkString,
  )
import Data.Semigroup ((<>))
import Data.Text (pack)

constRes :: Applicative m => a -> b -> m a
constRes = const . pure

type Result e m a = Schema -> Resolver QUERY e m a

class RenderSchema a where
  render :: (Monad m) => a -> Schema -> Resolver QUERY e m (ResModel QUERY e m)

instance RenderSchema (TypeDefinition a) where
  render TypeDefinition {typeName, typeMeta, typeContent} = __render typeContent
    where
      __render ::
        (Monad m) => TypeContent bool a -> Schema -> Resolver QUERY e m (ResModel QUERY e m)
      __render DataScalar {} =
        constRes $ createLeafType SCALAR typeName typeMeta Nothing
      __render (DataEnum enums) =
        constRes $
          createLeafType ENUM typeName typeMeta (Just $ map createEnumValue enums)
      -- __render (DataInputObject fields) = \lib ->
      --   createInputObject typeName typeMeta
      --     <$> traverse (`renderinputValue` lib) (elems fields)
      __render DataObject {objectImplements, objectFields} =
        pure . createObjectType typeName typeMeta objectImplements objectFields
      __render (DataUnion union) = \schema ->
        pure $ typeFromUnion schema (typeName, typeMeta, union)
      __render (DataInputUnion members) =
        renderInputUnion (typeName, typeMeta, members)
      __render (DataInterface fields) =
        renderInterface typeName Nothing fields

renderFields :: Monad m => Schema -> FieldsDefinition cat -> Resolver QUERY e m [ResModel QUERY e m]
renderFields schema = traverse (`render` schema) . filter fieldVisibility . elems

renderInterface ::
  Monad m => TypeName -> Maybe Meta -> FieldsDefinition OUT -> Schema -> Resolver QUERY e m (ResModel QUERY e m)
renderInterface name meta fields schema =
  pure $
    mkObject
      "__Type"
      [ renderKind INTERFACE,
        renderName name,
        description meta,
        ("fields", mkList <$> renderFields schema fields),
        ("possibleTypes", mkList <$> interfacePossibleTypes schema name)
      ]

interfacePossibleTypes ::
  (Monad m) =>
  Schema ->
  TypeName ->
  Resolver QUERY e m [ResModel QUERY e m]
interfacePossibleTypes schema interfaceName = sequence $ concatMap implements (elems schema)
  where
    implements typeDef@TypeDefinition {typeContent = DataObject {objectImplements}, ..}
      | interfaceName `elem` objectImplements = [render typeDef schema]
    implements _ = []

createEnumValue :: Monad m => DataEnumValue -> ResModel QUERY e m
createEnumValue DataEnumValue {enumName, enumMeta} =
  mkObject "__Field" $
    [ renderName enumName,
      description enumMeta
    ]
      <> renderDeprecated enumMeta

renderDeprecated ::
  (Monad m) =>
  Maybe Meta ->
  [(FieldName, Resolver QUERY e m (ResModel QUERY e m))]
renderDeprecated meta =
  [ ("isDeprecated", pure $ mkBoolean (isJust $ meta >>= lookupDeprecated)),
    ("deprecationReason", opt (pure . mkString) (meta >>= lookupDeprecated >>= lookupDeprecatedReason))
  ]

description :: Monad m => Maybe Meta -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
description enumMeta = ("description", opt (pure . mkString) (enumMeta >>= metaDescription))

renderArguments :: (Monad m) => ArgumentsDefinition -> Schema -> Resolver QUERY e m [ResModel QUERY e m]
renderArguments ArgumentsDefinition {arguments} lib = traverse (`renderinputValue` lib) $ elems arguments
renderArguments NoArguments _ = pure []

instance RenderSchema (FieldDefinition OUT) where
  render field@FieldDefinition {fieldName, fieldType = TypeRef {typeConName}, fieldArgs, fieldMeta} lib =
    do
      kind <- renderTypeKind <$> lookupKind typeConName lib
      pure
        $ mkObject "__Field"
        $ [ renderFieldName fieldName,
            description fieldMeta,
            ("args", mkList <$> renderArguments fieldArgs lib),
            ("type'", pure (withTypeWrapper field $ createType kind typeConName Nothing $ Just []))
          ]
          <> renderDeprecated fieldMeta

renderTypeKind :: DataTypeKind -> TypeKind
renderTypeKind KindScalar = SCALAR
renderTypeKind (KindObject _) = OBJECT
renderTypeKind KindUnion = UNION
renderTypeKind KindInputUnion = INPUT_OBJECT
renderTypeKind KindEnum = ENUM
renderTypeKind KindInputObject = INPUT_OBJECT
renderTypeKind KindList = LIST
renderTypeKind KindNonNull = NON_NULL
renderTypeKind KindInterface = INTERFACE

lookupKind :: (Monad m) => TypeName -> Result e m DataTypeKind
lookupKind name schema = kindOf <$> selectBy ("Kind Not Found: " <> msg name) name schema

renderinputValue ::
  (Monad m) =>
  FieldDefinition IN ->
  Result e m (ResModel QUERY e m)
renderinputValue input = fmap (createInputValueWith (fieldName input) (fieldMeta input)) . createInputObjectType input

createInputObjectType ::
  (Monad m) => FieldDefinition IN -> Result e m (ResModel QUERY e m)
createInputObjectType field@FieldDefinition {fieldType = TypeRef {typeConName}} lib =
  do
    kind <- renderTypeKind <$> lookupKind typeConName lib
    pure $ withTypeWrapper field $ createType kind typeConName Nothing $ Just []

renderInputUnion ::
  (Monad m) =>
  (TypeName, Maybe Meta, DataInputUnion) ->
  Result e m (ResModel QUERY e m)
renderInputUnion (key, meta, fields) lib =
  createInputObject key meta
    <$> traverse
      createField
      (createInputUnionFields key $ map fst $ filter snd fields)
  where
    createField field =
      createInputValueWith (fieldName field) Nothing <$> createInputObjectType field lib

createLeafType ::
  Monad m =>
  TypeKind ->
  TypeName ->
  Maybe Meta ->
  Maybe [ResModel QUERY e m] ->
  ResModel QUERY e m
createLeafType kind name meta enums =
  mkObject
    "__Type"
    [ renderKind kind,
      renderName name,
      description meta,
      ("enumValues", optList enums)
    ]

typeFromUnion :: Monad m => Schema -> (TypeName, Maybe Meta, DataUnion) -> ResModel QUERY e m
typeFromUnion schema (name, typeMeta, typeContent) =
  mkObject
    "__Type"
    [ renderKind UNION,
      renderName name,
      description typeMeta,
      ("possibleTypes", mkList <$> traverse (unionPossibleType schema) typeContent)
    ]

unionPossibleType :: Monad m => Schema -> TypeName -> Resolver QUERY e m (ResModel QUERY e m)
unionPossibleType schema name =
  selectBy (" INTERNAL: INTROSPECTION Type not Found: \"" <> msg name <> "\"") name schema
    >>= (`render` schema)

createObjectType ::
  Monad m => TypeName -> Maybe Meta -> [TypeName] -> FieldsDefinition OUT -> Schema -> ResModel QUERY e m
createObjectType name meta interfaces fields schema =
  mkObject
    "__Type"
    [ renderKind OBJECT,
      renderName name,
      description meta,
      ("fields", mkList <$> renderFields schema fields),
      ("interfaces", mkList <$> traverse (implementedInterface schema) interfaces)
    ]

implementedInterface ::
  (Monad m) =>
  Schema ->
  TypeName ->
  Resolver QUERY e m (ResModel QUERY e m)
implementedInterface schema name =
  selectBy ("INTERNAL: cant found  Interface " <> msg name) name schema
    >>= __render
  where
    __render typeDef@TypeDefinition {typeContent = DataInterface {}} = render typeDef schema
    __render _ = failure ("Type " <> msg name <> " must be an Interface" :: Message)

optList :: Monad m => Maybe [ResModel QUERY e m] -> Resolver QUERY e m (ResModel QUERY e m)
optList = pure . maybe mkNull mkList

createInputObject ::
  Monad m => TypeName -> Maybe Meta -> [ResModel QUERY e m] -> ResModel QUERY e m
createInputObject name meta fields =
  mkObject
    "__Type"
    [ renderKind INPUT_OBJECT,
      renderName name,
      description meta,
      ("inputFields", pure $ mkList fields)
    ]

createType ::
  Monad m =>
  TypeKind ->
  TypeName ->
  Maybe Meta ->
  Maybe [ResModel QUERY e m] ->
  ResModel QUERY e m
createType kind name desc fields =
  mkObject
    "__Type"
    [ renderKind kind,
      renderName name,
      description desc,
      ("fields", pure $ maybe mkNull mkList fields),
      ("enumValues", pure $ mkList [])
    ]

opt :: Monad m => (a -> Resolver QUERY e m (ResModel QUERY e m)) -> Maybe a -> Resolver QUERY e m (ResModel QUERY e m)
opt f (Just x) = f x
opt _ Nothing = pure mkNull

renderName :: Monad m => TypeName -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderName = ("name",) . pure . mkString . readTypeName

renderFieldName :: Monad m => FieldName -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderFieldName = ("name",) . pure . mkString . convertToJSONName

renderKind :: Monad m => TypeKind -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderKind = ("kind",) . pure . mkString . pack . show

withTypeWrapper :: Monad m => FieldDefinition cat -> ResModel QUERY e m -> ResModel QUERY e m
withTypeWrapper FieldDefinition {fieldType = TypeRef {typeWrappers}} typ =
  foldr wrapAs typ (toGQLWrapper typeWrappers)

wrapAs :: Monad m => DataTypeWrapper -> ResModel QUERY e m -> ResModel QUERY e m
wrapAs wrapper contentType =
  mkObject
    "__Type"
    [ renderKind (kind wrapper),
      ("ofType", pure contentType)
    ]
  where
    kind ListType = LIST
    kind NonNullType = NON_NULL

createInputValueWith ::
  Monad m => FieldName -> Maybe Meta -> ResModel QUERY e m -> ResModel QUERY e m
createInputValueWith name meta ivType =
  mkObject
    "__InputValue"
    [ renderFieldName name,
      description meta,
      ("type'", pure ivType)
    ]
