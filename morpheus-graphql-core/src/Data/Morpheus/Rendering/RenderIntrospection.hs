{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
  ( Failure,
    elems,
    failure,
    fromElems,
    selectBy,
    selectOr,
  )
import qualified Data.Morpheus.Rendering.RenderGQL as GQL (RenderGQL (..))
import Data.Morpheus.Schema.TypeKind (TypeKind (..))
import qualified Data.Morpheus.Types.Internal.AST as AST (TypeKind (..))
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
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
    GQLErrors,
    IN,
    Message,
    OUT,
    Object,
    ObjectEntry (..),
    QUERY,
    RESOLVED,
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    VALID,
    Value (..),
    createInputUnionFields,
    fieldVisibility,
    kindOf,
    lookupDeprecated,
    lookupDeprecatedReason,
    msg,
    toGQLWrapper,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Context (..),
    ResModel,
    Resolver,
    mkBoolean,
    mkList,
    mkNull,
    mkObject,
    mkString,
    unsafeInternalContext,
  )
import Data.Semigroup ((<>))
import Data.Text (pack)

type Result e m a = Resolver QUERY e m a

class
  ( Monad m,
    Failure Message m,
    Failure GQLErrors m
  ) =>
  WithSchema m
  where
  getSchema :: m Schema

instance Monad m => WithSchema (Resolver QUERY e m) where
  getSchema = schema <$> unsafeInternalContext

selectType ::
  WithSchema m =>
  TypeName ->
  m (TypeDefinition ANY)
selectType name =
  getSchema
    >>= selectBy (" INTERNAL: INTROSPECTION Type not Found: \"" <> msg name <> "\"") name

class RenderSchema a where
  render ::
    (Monad m) =>
    a ->
    Resolver QUERY e m (ResModel QUERY e m)

instance RenderSchema TypeName where
  render = pure . mkString . readTypeName

instance RenderSchema FieldName where
  render = pure . mkString . readName

instance RenderSchema Description where
  render = pure . mkString

instance RenderSchema TypeKind where
  render = pure . mkString . pack . show

instance RenderSchema a => RenderSchema [a] where
  render ls = mkList <$> traverse render ls

instance RenderSchema DirectiveDefinition where
  render
    DirectiveDefinition
      { directiveDefinitionName,
        directiveDefinitionDescription,
        directiveDefinitionLocations,
        directiveDefinitionArgs
      } =
      pure $
        mkObject
          "__Directive"
          [ renderName directiveDefinitionName,
            description directiveDefinitionDescription,
            ("locations", render directiveDefinitionLocations),
            ("args", render directiveDefinitionArgs)
          ]

instance RenderSchema DirectiveLocation where
  render locations = pure $ mkString (pack $ show locations)

instance RenderSchema (TypeDefinition a) where
  render
    TypeDefinition
      { typeName,
        typeDescription,
        typeContent
      } = __render typeContent
      where
        __render ::
          (Monad m) => TypeContent bool a -> Resolver QUERY e m (ResModel QUERY e m)
        __render DataScalar {} =
          createLeafType SCALAR typeName typeDescription Nothing
        __render (DataEnum enums) =
          traverse render enums
            >>= createLeafType ENUM typeName typeDescription . Just
        __render (DataInputObject fields) =
          createInputObject typeName typeDescription
            <$> traverse render (elems fields)
        __render DataObject {objectImplements, objectFields} =
          createObjectType typeName typeDescription objectImplements objectFields
        __render (DataUnion union) =
          typeFromUnion typeName typeDescription union
        __render (DataInputUnion members) =
          renderInputUnion (typeName, typeDescription, members)
        __render (DataInterface fields) =
          renderInterface typeName Nothing fields

instance RenderSchema (FieldsDefinition OUT) where
  render = fmap mkList . traverse render . filter fieldVisibility . elems

instance RenderSchema (FieldDefinition OUT) where
  render
    field@FieldDefinition
      { fieldName,
        fieldType = TypeRef {typeConName},
        fieldContent,
        fieldDescription,
        fieldDirectives
      } =
      do
        kind <- lookupKind typeConName
        pure
          $ mkObject "__Field"
          $ [ renderName fieldName,
              description fieldDescription,
              ("args", maybe (pure $ mkList []) render fieldContent),
              ("type", pure (withTypeWrapper field $ createType kind typeConName Nothing $ Just []))
            ]
            <> renderDeprecated fieldDirectives

instance RenderSchema (FieldContent TRUE OUT) where
  render (FieldArgs args) = render args

instance RenderSchema ArgumentsDefinition where
  render ArgumentsDefinition {arguments} = mkList <$> traverse render (elems arguments)

instance RenderSchema (FieldDefinition IN) where
  render input@FieldDefinition {..} =
    createInputValueWith
      fieldName
      fieldType
      fieldDescription
      (fmap defaultInputValue fieldContent)
      <$> createInputObjectType input

instance RenderSchema DataEnumValue where
  render DataEnumValue {enumName, enumDescription, enumDirectives} =
    pure $ mkObject "__Field" $
      [ renderName enumName,
        description enumDescription
      ]
        <> renderDeprecated enumDirectives

renderInterface ::
  Monad m => TypeName -> Maybe Description -> FieldsDefinition OUT -> Resolver QUERY e m (ResModel QUERY e m)
renderInterface name desc fields =
  pure $ mkType INTERFACE name desc [("fields", render fields), ("possibleTypes", mkList <$> interfacePossibleTypes name)]

interfacePossibleTypes ::
  (Monad m) =>
  TypeName ->
  Resolver QUERY e m [ResModel QUERY e m]
interfacePossibleTypes interfaceName =
  getSchema
    >>= sequence
      . concatMap implements
      . elems
  where
    implements typeDef@TypeDefinition {typeContent = DataObject {objectImplements}, ..}
      | interfaceName `elem` objectImplements = [render typeDef]
    implements _ = []

renderDeprecated ::
  (Monad m) =>
  Directives VALID ->
  [(FieldName, Resolver QUERY e m (ResModel QUERY e m))]
renderDeprecated dirs =
  [ ("isDeprecated", pure $ mkBoolean (isJust $ lookupDeprecated dirs)),
    ("deprecationReason", opt (pure . mkString) (lookupDeprecated dirs >>= lookupDeprecatedReason))
  ]

description :: Monad m => Maybe Description -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
description desc = ("description", opt render desc)

lookupKind :: (Monad m) => TypeName -> Result e m TypeKind
lookupKind = fmap (renderTypeKind . kindOf) . selectType

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

createInputObjectType ::
  (Monad m) => FieldDefinition IN -> Result e m (ResModel QUERY e m)
createInputObjectType field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  do
    kind <- lookupKind typeConName
    pure $ withTypeWrapper field $ createType kind typeConName Nothing $ Just []

renderInputUnion ::
  (Monad m) =>
  (TypeName, Maybe Description, DataInputUnion) ->
  Result e m (ResModel QUERY e m)
renderInputUnion (key, meta, fields) =
  createInputObject key meta
    <$> traverse
      createField
      (createInputUnionFields key $ map fst $ filter snd fields)
  where
    createField field@FieldDefinition {fieldType} =
      createInputValueWith (fieldName field) fieldType Nothing Nothing <$> createInputObjectType field

mkType ::
  (Monad m, RenderSchema name) =>
  TypeKind ->
  name ->
  Maybe Description ->
  [(FieldName, Resolver QUERY e m (ResModel QUERY e m))] ->
  ResModel QUERY e m
mkType kind name desc etc =
  mkObject
    "__Type"
    ( [ renderKind kind,
        renderName name,
        description desc
      ]
        <> etc
    )

createLeafType ::
  Monad m =>
  TypeKind ->
  TypeName ->
  Maybe Description ->
  Maybe [ResModel QUERY e m] ->
  Result e m (ResModel QUERY e m)
createLeafType kind name desc enums = pure $ mkType kind name desc [("enumValues", optList enums)]

typeFromUnion :: Monad m => TypeName -> Maybe Description -> DataUnion -> Result e m (ResModel QUERY e m)
typeFromUnion name desc typeContent =
  pure $ mkType UNION name desc [("possibleTypes", mkList <$> traverse unionPossibleType typeContent)]

unionPossibleType :: Monad m => TypeName -> Resolver QUERY e m (ResModel QUERY e m)
unionPossibleType name = selectType name >>= render

createObjectType ::
  Monad m => TypeName -> Maybe Description -> [TypeName] -> FieldsDefinition OUT -> Result e m (ResModel QUERY e m)
createObjectType name desc interfaces fields =
  pure $ mkType OBJECT name desc [("fields", render fields), ("interfaces", mkList <$> traverse implementedInterface interfaces)]

implementedInterface ::
  (Monad m) =>
  TypeName ->
  Resolver QUERY e m (ResModel QUERY e m)
implementedInterface name =
  selectType name
    >>= __render
  where
    __render typeDef@TypeDefinition {typeContent = DataInterface {}} = render typeDef
    __render _ = failure ("Type " <> msg name <> " must be an Interface" :: Message)

optList :: Monad m => Maybe [ResModel QUERY e m] -> Resolver QUERY e m (ResModel QUERY e m)
optList = pure . maybe mkNull mkList

createInputObject ::
  Monad m => TypeName -> Maybe Description -> [ResModel QUERY e m] -> ResModel QUERY e m
createInputObject name desc fields = mkType INPUT_OBJECT name desc [("inputFields", pure $ mkList fields)]

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

renderName ::
  ( RenderSchema name,
    Monad m
  ) =>
  name ->
  (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderName = ("name",) . render

renderKind :: Monad m => TypeKind -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderKind = ("kind",) . render

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

defaultValue ::
  Monad m =>
  TypeRef ->
  Maybe (Value RESOLVED) ->
  ( FieldName,
    Resolver QUERY e m (ResModel QUERY e m)
  )
defaultValue
  typeRef
  value =
    ( "defaultValue",
      opt
        ( fmap
            (mkString . GQL.render)
            . fulfill typeRef
            . Just
        )
        value
    )

fulfill ::
  WithSchema m =>
  TypeRef ->
  Maybe (Value RESOLVED) ->
  m (Value RESOLVED)
fulfill TypeRef {typeConName} (Just (Object fields)) =
  selectType typeConName
    >>= \case
      TypeDefinition
        { typeContent =
            DataInputObject {inputObjectFields}
        } ->
          Object
            <$> ( traverse
                    (handleField fields)
                    (elems inputObjectFields)
                    >>= fromElems
                )
      _ -> failure (msg typeConName <> "is not must be Object")
fulfill typeRef (Just (List values)) =
  List <$> traverse (fulfill typeRef . Just) values
fulfill _ (Just v) = pure v
fulfill _ Nothing = pure Null

handleField ::
  WithSchema m =>
  Object RESOLVED ->
  FieldDefinition IN ->
  m (ObjectEntry RESOLVED)
handleField
  fields
  FieldDefinition
    { fieldName,
      fieldType,
      fieldContent = x
    } =
    ObjectEntry fieldName
      <$> fulfill
        fieldType
        ( selectOr
            (fmap defaultInputValue x)
            (Just . entryValue)
            fieldName
            fields
        )

createInputValueWith ::
  Monad m =>
  FieldName ->
  TypeRef ->
  Maybe Description ->
  Maybe (Value RESOLVED) ->
  ResModel QUERY e m ->
  ResModel QUERY e m
createInputValueWith name tyRef desc value ivType =
  mkObject
    "__InputValue"
    [ renderName name,
      description desc,
      ("type", pure ivType),
      defaultValue tyRef value
    ]
