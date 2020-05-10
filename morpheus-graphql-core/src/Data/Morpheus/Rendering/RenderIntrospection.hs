{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Rendering.RenderIntrospection
  ( render,
    createObjectType,
  )
where

import Data.Maybe (isJust)
-- Morpheus

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
    FieldsDefinition,
    Meta (..),
    Name,
    QUERY,
    ScalarValue (..),
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    convertToJSONName,
    createInputUnionFields,
    fieldVisibility,
    kindOf,
    lookupDataType,
    lookupDeprecated,
    lookupDeprecatedReason,
    toGQLWrapper,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Listable (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Failure (..),
    ObjectResModel (..),
    ResModel (..),
    Resolver,
  )
import Data.Semigroup ((<>))
import Data.Text (Text, pack)

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
      __render (DataInputObject fields) = \lib ->
        createInputObject typeName typeMeta
          <$> traverse (`renderinputValue` lib) (toList fields)
      __render DataObject {objectFields} = \schema ->
        createObjectType typeName typeMeta
          <$> (Just <$> renderFields schema objectFields)
      __render (DataUnion union) =
        constRes $ typeFromUnion (typeName, typeMeta, union)
      __render (DataInputUnion members) =
        renderInputUnion (typeName, typeMeta, members)
      __render (DataInterface fields) = \schema -> do
        fields' <- renderFields schema fields
        renderInterface typeName Nothing fields' schema

renderFields :: Monad m => Schema -> FieldsDefinition -> Resolver QUERY e m [ResModel QUERY e m]
renderFields schema = traverse (`render` schema) . filter fieldVisibility . toList

renderInterface ::
  Monad m => Text -> Maybe Meta -> [ResModel QUERY e m] -> Schema -> Resolver QUERY e m (ResModel QUERY e m)
renderInterface name meta fields _ =
  pure $
    object
      "__Type"
      [ renderKind INTERFACE,
        renderName name,
        description meta,
        ("fields", pure $ ResList fields),
        ("possibleTypes", pure $ ResList []) -- TODO: list of all objects that implements interface
      ]

createEnumValue :: Monad m => DataEnumValue -> ResModel QUERY e m
createEnumValue DataEnumValue {enumName, enumMeta} =
  object "__Field" $
    [ renderName enumName,
      description enumMeta
    ]
      <> renderDeprecated enumMeta

renderDeprecated ::
  (Monad m) =>
  Maybe Meta ->
  [(Name, Resolver QUERY e m (ResModel QUERY e m))]
renderDeprecated meta =
  [ ("isDeprecated", pure $ ResScalar $ Boolean (isJust $ meta >>= lookupDeprecated)),
    ("deprecationReason", opt string (meta >>= lookupDeprecated >>= lookupDeprecatedReason))
  ]

description :: Monad m => Maybe Meta -> (Name, Resolver QUERY e m (ResModel QUERY e m))
description enumMeta = ("description", opt string (enumMeta >>= metaDescription))

renderArguments :: (Monad m) => ArgumentsDefinition -> Schema -> Resolver QUERY e m [ResModel QUERY e m]
renderArguments ArgumentsDefinition {arguments} lib = traverse (`renderinputValue` lib) $ toList arguments
renderArguments NoArguments _ = pure []

instance RenderSchema FieldDefinition where
  render field@FieldDefinition {fieldName, fieldType = TypeRef {typeConName}, fieldArgs, fieldMeta} lib =
    do
      kind <- renderTypeKind <$> lookupKind typeConName lib
      pure
        $ object "__Field"
        $ [ renderName fieldName,
            description fieldMeta,
            ("args", ResList <$> renderArguments fieldArgs lib),
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

lookupKind :: (Monad m) => Text -> Result e m DataTypeKind
lookupKind name lib = case lookupDataType name lib of
  Nothing -> failure $ "Kind Not Found: " <> name
  Just value -> pure (kindOf value)

renderinputValue ::
  (Monad m) =>
  FieldDefinition ->
  Result e m (ResModel QUERY e m)
renderinputValue input = fmap (createInputValueWith (fieldName input) (fieldMeta input)) . createInputObjectType input

createInputObjectType ::
  (Monad m) => FieldDefinition -> Result e m (ResModel QUERY e m)
createInputObjectType field@FieldDefinition {fieldType = TypeRef {typeConName}} lib =
  do
    kind <- renderTypeKind <$> lookupKind typeConName lib
    pure $ withTypeWrapper field $ createType kind typeConName Nothing $ Just []

renderInputUnion ::
  (Monad m) =>
  (Text, Maybe Meta, DataInputUnion) ->
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
  Text ->
  Maybe Meta ->
  Maybe [ResModel QUERY e m] ->
  ResModel QUERY e m
createLeafType kind name meta enums =
  object
    "__Type"
    [ renderKind kind,
      renderName name,
      description meta,
      ("enumValues", optList enums)
    ]

typeFromUnion :: Monad m => (Text, Maybe Meta, DataUnion) -> ResModel QUERY e m
typeFromUnion (name, typeMeta, typeContent) =
  object
    "__Type"
    [ renderKind UNION,
      renderName name,
      description typeMeta,
      ("possibleTypes", pure $ ResList (map (\x -> createObjectType x Nothing $ Just []) typeContent))
    ]

createObjectType ::
  Monad m => Text -> Maybe Meta -> Maybe [ResModel QUERY e m] -> ResModel QUERY e m
createObjectType name meta fields =
  object
    "__Type"
    [ renderKind OBJECT,
      renderName name,
      description meta,
      ("fields", optList fields),
      ("interfaces", pure $ ResList []) -- TODO: list of all implemented interfaces
    ]

optList :: Monad m => Maybe [ResModel QUERY e m] -> Resolver QUERY e m (ResModel QUERY e m)
optList = pure . maybe ResNull ResList

createInputObject ::
  Monad m => Text -> Maybe Meta -> [ResModel QUERY e m] -> ResModel QUERY e m
createInputObject name meta fields =
  object
    "__Type"
    [ renderKind INPUT_OBJECT,
      renderName name,
      description meta,
      ("inputFields", pure $ ResList fields)
    ]

createType ::
  Monad m =>
  TypeKind ->
  Text ->
  Maybe Meta ->
  Maybe [ResModel QUERY e m] ->
  ResModel QUERY e m
createType kind name desc fields =
  object
    "__Type"
    [ renderKind kind,
      renderName name,
      description desc,
      ("fields", pure $ maybe ResNull ResList fields),
      ("enumValues", pure $ ResList [])
    ]

opt :: Monad m => (a -> Resolver QUERY e m (ResModel QUERY e m)) -> Maybe a -> Resolver QUERY e m (ResModel QUERY e m)
opt f (Just x) = f x
opt _ Nothing = pure ResNull

string :: Monad m => Text -> Resolver QUERY e m (ResModel QUERY e m)
string = pure . ResScalar . String

renderName :: Monad m => Text -> (Name, Resolver QUERY e m (ResModel QUERY e m))
renderName = ("name",) . string . convertToJSONName

renderKind :: Monad m => TypeKind -> (Name, Resolver QUERY e m (ResModel QUERY e m))
renderKind = ("kind",) . string . pack . show

object ::
  Name ->
  [(Name, Resolver QUERY e m (ResModel QUERY e m))] ->
  ResModel QUERY e m
object __typename objectFields =
  ResObject
    ( ObjectResModel
        { __typename,
          objectFields
        }
    )

withTypeWrapper :: Monad m => FieldDefinition -> ResModel QUERY e m -> ResModel QUERY e m
withTypeWrapper FieldDefinition {fieldType = TypeRef {typeWrappers}} typ =
  foldr wrapAs typ (toGQLWrapper typeWrappers)

wrapAs :: Monad m => DataTypeWrapper -> ResModel QUERY e m -> ResModel QUERY e m
wrapAs wrapper contentType =
  object
    "__Type"
    [ renderKind (kind wrapper),
      ("ofType", pure contentType)
    ]
  where
    kind ListType = LIST
    kind NonNullType = NON_NULL

createInputValueWith ::
  Monad m => Text -> Maybe Meta -> ResModel QUERY e m -> ResModel QUERY e m
createInputValueWith name meta ivType =
  object
    "__InputValue"
    [ renderName name,
      description meta,
      ("type'", pure ivType)
    ]
