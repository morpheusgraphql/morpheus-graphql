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
        __render DataScalar {} = pure $ mkType SCALAR typeName typeDescription []
        __render (DataEnum enums) =
          pure $ mkType ENUM typeName typeDescription [("enumValues", render enums)]
        __render (DataInputObject fields) =
          createInputObject typeName typeDescription
            <$> traverse render (elems fields)
        __render DataObject {objectImplements, objectFields} =
          createObjectType typeName typeDescription objectImplements objectFields
        __render (DataUnion union) =
          pure $ mkType UNION typeName typeDescription [("possibleTypes", mkList <$> traverse unionPossibleType union)]
        __render (DataInputUnion members) =
          renderInputUnion (typeName, typeDescription, members)
        __render (DataInterface fields) =
          pure $ mkType INTERFACE typeName typeDescription [("fields", render fields), ("possibleTypes", mkList <$> interfacePossibleTypes typeName)]

instance RenderSchema (FieldsDefinition OUT) where
  render = fmap mkList . traverse render . filter fieldVisibility . elems

instance RenderSchema (FieldDefinition OUT) where
  render
    field@FieldDefinition
      { fieldName,
        fieldContent,
        fieldDescription,
        fieldDirectives
      } =
      pure
        $ mkObject "__Field"
        $ [ renderName fieldName,
            description fieldDescription,
            ("args", maybe (pure $ mkList []) render fieldContent),
            type' field
          ]
          <> renderDeprecated fieldDirectives

instance RenderSchema (FieldContent TRUE OUT) where
  render (FieldArgs args) = render args

instance RenderSchema ArgumentsDefinition where
  render ArgumentsDefinition {arguments} = mkList <$> traverse render (elems arguments)

instance RenderSchema (FieldDefinition IN) where
  render input@FieldDefinition {..} =
    pure $ mkInputValue input

instance RenderSchema DataEnumValue where
  render DataEnumValue {enumName, enumDescription, enumDirectives} =
    pure $ mkObject "__Field" $
      [ renderName enumName,
        description enumDescription
      ]
        <> renderDeprecated enumDirectives

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

renderInputUnion ::
  (Monad m) =>
  (TypeName, Maybe Description, DataInputUnion) ->
  Result e m (ResModel QUERY e m)
renderInputUnion (key, meta, fields) =
  pure $ createInputObject key meta $
    map
      mkInputValue
      (createInputUnionFields key $ map fst $ filter snd fields)

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

createInputObject ::
  Monad m => TypeName -> Maybe Description -> [ResModel QUERY e m] -> ResModel QUERY e m
createInputObject name desc fields = mkType INPUT_OBJECT name desc [("inputFields", pure $ mkList fields)]

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

type' :: Monad m => FieldDefinition cat -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
type' = ("type",) . mkTypeRef

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

mkInputValue ::
  Monad m =>
  FieldDefinition IN ->
  ResModel QUERY e m
mkInputValue field@FieldDefinition {..} =
  mkObject
    "__InputValue"
    [ renderName fieldName,
      description fieldDescription,
      type' field,
      defaultValue fieldType (fmap defaultInputValue fieldContent)
    ]

mkTypeRef ::
  (Monad m) => FieldDefinition cat -> Result e m (ResModel QUERY e m)
mkTypeRef field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  do
    kind <- lookupKind typeConName
    pure $ withTypeWrapper field $ mkType kind typeConName Nothing []
