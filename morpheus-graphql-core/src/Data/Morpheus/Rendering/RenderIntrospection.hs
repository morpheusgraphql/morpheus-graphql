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
import qualified Data.Morpheus.Types.Internal.AST as AST (TypeKind (..))
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    DataEnumValue (..),
    DataInputUnion,
    DataInputUnion,
    DataTypeWrapper (..),
    DataUnion,
    Description,
    DirectiveDefinition (..),
    DirectiveLocation,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    IN,
    Message,
    OUT,
    QUERY,
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    VALID,
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

instance RenderSchema DirectiveDefinition where
  render
    DirectiveDefinition
      { directiveDefinitionName,
        directiveDefinitionDescription,
        directiveDefinitionLocations,
        directiveDefinitionArgs
      }
    schema =
      pure $
        mkObject
          "__Directive"
          [ renderFieldName directiveDefinitionName,
            description directiveDefinitionDescription,
            ("locations", render directiveDefinitionLocations schema),
            ("args", mkList <$> renderArguments directiveDefinitionArgs schema)
          ]

instance RenderSchema a => RenderSchema [a] where
  render ls schema = mkList <$> traverse (`render` schema) ls

instance RenderSchema DirectiveLocation where
  render locations _ = pure $ mkString (pack $ show locations)

instance RenderSchema (TypeDefinition a) where
  render TypeDefinition {typeName, typeDescription, typeContent} = __render typeContent
    where
      __render ::
        (Monad m) => TypeContent bool a -> Schema -> Resolver QUERY e m (ResModel QUERY e m)
      __render DataScalar {} =
        constRes $ createLeafType SCALAR typeName typeDescription Nothing
      __render (DataEnum enums) =
        constRes $
          createLeafType ENUM typeName typeDescription (Just $ map createEnumValue enums)
      __render (DataInputObject fields) = \lib ->
        createInputObject typeName typeDescription
          <$> traverse (`renderinputValue` lib) (elems fields)
      __render DataObject {objectImplements, objectFields} =
        pure . createObjectType typeName typeDescription objectImplements objectFields
      __render (DataUnion union) = \schema ->
        pure $ typeFromUnion schema (typeName, typeDescription, union)
      __render (DataInputUnion members) =
        renderInputUnion (typeName, typeDescription, members)
      __render (DataInterface fields) =
        renderInterface typeName Nothing fields

renderFields :: Monad m => Schema -> FieldsDefinition cat -> Resolver QUERY e m [ResModel QUERY e m]
renderFields schema = traverse (`render` schema) . filter fieldVisibility . elems

renderInterface ::
  Monad m => TypeName -> Maybe Description -> FieldsDefinition OUT -> Schema -> Resolver QUERY e m (ResModel QUERY e m)
renderInterface name desc fields schema =
  pure $
    mkObject
      "__Type"
      [ renderKind INTERFACE,
        renderName name,
        description desc,
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
createEnumValue DataEnumValue {enumName, enumDescription, enumDirectives} =
  mkObject "__Field" $
    [ renderName enumName,
      description enumDescription
    ]
      <> renderDeprecated enumDirectives

renderDeprecated ::
  (Monad m) =>
  Directives VALID ->
  [(FieldName, Resolver QUERY e m (ResModel QUERY e m))]
renderDeprecated dirs =
  [ ("isDeprecated", pure $ mkBoolean (isJust $ lookupDeprecated dirs)),
    ("deprecationReason", opt (pure . mkString) (lookupDeprecated dirs >>= lookupDeprecatedReason))
  ]

description :: Monad m => Maybe Description -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
description desc = ("description", opt (pure . mkString) desc)

renderArguments :: (Monad m) => ArgumentsDefinition -> Schema -> Resolver QUERY e m [ResModel QUERY e m]
renderArguments ArgumentsDefinition {arguments} lib = traverse (`renderinputValue` lib) $ elems arguments

instance RenderSchema (FieldDefinition cat) where
  render
    field@FieldDefinition
      { fieldName,
        fieldType = TypeRef {typeConName},
        fieldContent,
        fieldDescription,
        fieldDirectives
      }
    lib =
      do
        kind <- lookupKind typeConName lib
        pure
          $ mkObject "__Field"
          $ [ renderFieldName fieldName,
              description fieldDescription,
              ("args", mkList <$> renderFieldArgs fieldContent lib),
              ("type", pure (withTypeWrapper field $ createType kind typeConName Nothing $ Just []))
            ]
            <> renderDeprecated fieldDirectives

renderFieldArgs :: (Monad m) => FieldContent TRUE cat -> Schema -> Resolver QUERY e m [ResModel QUERY e m]
renderFieldArgs (FieldArgs args) lib = renderArguments args lib
renderFieldArgs _ _ = pure []

lookupKind :: (Monad m) => TypeName -> Result e m TypeKind
lookupKind name schema = renderTypeKind . kindOf <$> selectBy ("Kind Not Found: " <> msg name) name schema

renderTypeKind :: AST.TypeKind -> TypeKind
renderTypeKind AST.KindScalar = SCALAR
renderTypeKind (AST.KindObject _) = OBJECT
renderTypeKind AST.KindUnion = UNION
renderTypeKind AST.KindInputUnion = INPUT_OBJECT
renderTypeKind AST.KindEnum = ENUM
renderTypeKind AST.KindInputObject = INPUT_OBJECT
renderTypeKind AST.KindList = LIST
renderTypeKind AST.KindNonNull = NON_NULL
renderTypeKind AST.KindInterface = INTERFACE

renderinputValue ::
  (Monad m) =>
  FieldDefinition IN ->
  Result e m (ResModel QUERY e m)
renderinputValue input = fmap (createInputValueWith (fieldName input) (fieldDescription input)) . createInputObjectType input

createInputObjectType ::
  (Monad m) => FieldDefinition IN -> Result e m (ResModel QUERY e m)
createInputObjectType field@FieldDefinition {fieldType = TypeRef {typeConName}} lib =
  do
    kind <- lookupKind typeConName lib
    pure $ withTypeWrapper field $ createType kind typeConName Nothing $ Just []

renderInputUnion ::
  (Monad m) =>
  (TypeName, Maybe Description, DataInputUnion) ->
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
  Maybe Description ->
  Maybe [ResModel QUERY e m] ->
  ResModel QUERY e m
createLeafType kind name desc enums =
  mkObject
    "__Type"
    [ renderKind kind,
      renderName name,
      description desc,
      ("enumValues", optList enums)
    ]

typeFromUnion :: Monad m => Schema -> (TypeName, Maybe Description, DataUnion) -> ResModel QUERY e m
typeFromUnion schema (name, desc, typeContent) =
  mkObject
    "__Type"
    [ renderKind UNION,
      renderName name,
      description desc,
      ("possibleTypes", mkList <$> traverse (unionPossibleType schema) typeContent)
    ]

unionPossibleType :: Monad m => Schema -> TypeName -> Resolver QUERY e m (ResModel QUERY e m)
unionPossibleType schema name =
  selectBy (" INTERNAL: INTROSPECTION Type not Found: \"" <> msg name <> "\"") name schema
    >>= (`render` schema)

createObjectType ::
  Monad m => TypeName -> Maybe Description -> [TypeName] -> FieldsDefinition OUT -> Schema -> ResModel QUERY e m
createObjectType name desc interfaces fields schema =
  mkObject
    "__Type"
    [ renderKind OBJECT,
      renderName name,
      description desc,
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
  Monad m => TypeName -> Maybe Description -> [ResModel QUERY e m] -> ResModel QUERY e m
createInputObject name desc fields =
  mkObject
    "__Type"
    [ renderKind INPUT_OBJECT,
      renderName name,
      description desc,
      ("inputFields", pure $ mkList fields)
    ]

createType ::
  Monad m =>
  TypeKind ->
  TypeName ->
  Maybe Description ->
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
renderFieldName (FieldName name) = ("name", pure $ mkString name)

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
  Monad m => FieldName -> Maybe Description -> ResModel QUERY e m -> ResModel QUERY e m
createInputValueWith name desc ivType =
  mkObject
    "__InputValue"
    [ renderFieldName name,
      description desc,
      ("type", pure ivType)
    ]
