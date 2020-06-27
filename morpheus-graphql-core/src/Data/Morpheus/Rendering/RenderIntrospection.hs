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
    CONST,
    DataEnumValue (..),
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
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    UnionMember (..),
    VALID,
    Value (..),
    fieldVisibility,
    kindOf,
    lookupDeprecated,
    lookupDeprecatedReason,
    mkInputUnionFields,
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

class RenderIntrospection a where
  render ::
    (Monad m) =>
    a ->
    Resolver QUERY e m (ResModel QUERY e m)

instance RenderIntrospection TypeName where
  render = pure . mkString . readTypeName

instance RenderIntrospection FieldName where
  render = pure . mkString . readName

instance RenderIntrospection Description where
  render = pure . mkString

instance RenderIntrospection TypeKind where
  render = pure . mkString . pack . show

instance RenderIntrospection a => RenderIntrospection [a] where
  render ls = mkList <$> traverse render ls

instance RenderIntrospection DirectiveDefinition where
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

instance RenderIntrospection DirectiveLocation where
  render locations = pure $ mkString (pack $ show locations)

instance RenderIntrospection (TypeDefinition a) where
  render
    TypeDefinition
      { typeName,
        typeDescription,
        typeContent
      } = pure $ renderContent typeContent
      where
        __type :: Monad m => TypeKind -> [(FieldName, Resolver QUERY e m (ResModel QUERY e m))] -> ResModel QUERY e m
        __type kind = mkType kind typeName typeDescription
        renderContent :: Monad m => TypeContent bool a -> ResModel QUERY e m
        renderContent DataScalar {} = __type SCALAR []
        renderContent (DataEnum enums) = __type ENUM [("enumValues", render enums)]
        renderContent (DataInputObject inputFiels) =
          __type
            INPUT_OBJECT
            [("inputFields", render inputFiels)]
        renderContent DataObject {objectImplements, objectFields} =
          createObjectType typeName typeDescription objectImplements objectFields
        renderContent (DataUnion union) =
          __type
            UNION
            [("possibleTypes", render union)]
        renderContent (DataInputUnion members) =
          __type
            INPUT_OBJECT
            [ ( "inputFields",
                render
                  $ mkInputUnionFields typeName
                  $ filter visibility members
              )
            ]
        renderContent (DataInterface fields) =
          __type
            INTERFACE
            [ ("fields", render fields),
              ("possibleTypes", interfacePossibleTypes typeName)
            ]

instance RenderIntrospection (UnionMember OUT) where
  render UnionMember {memberName} = selectType memberName >>= render

instance RenderIntrospection (FieldDefinition cat) => RenderIntrospection (FieldsDefinition cat) where
  render = render . filter fieldVisibility . elems

instance RenderIntrospection (FieldDefinition OUT) where
  render FieldDefinition {..} =
    pure
      $ mkObject "__Field"
      $ [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          ("args", maybe (pure $ mkList []) render fieldContent)
        ]
        <> renderDeprecated fieldDirectives

instance RenderIntrospection (FieldContent TRUE OUT) where
  render (FieldArgs args) = render args

instance RenderIntrospection ArgumentsDefinition where
  render ArgumentsDefinition {arguments} = mkList <$> traverse render (elems arguments)

instance RenderIntrospection (FieldDefinition IN) where
  render FieldDefinition {..} =
    pure $
      mkObject
        "__InputValue"
        [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          defaultValue fieldType (fmap defaultInputValue fieldContent)
        ]

instance RenderIntrospection DataEnumValue where
  render DataEnumValue {enumName, enumDescription, enumDirectives} =
    pure $ mkObject "__Field" $
      [ renderName enumName,
        description enumDescription
      ]
        <> renderDeprecated enumDirectives

instance RenderIntrospection TypeRef where
  render TypeRef {typeConName, typeWrappers} = do
    kind <- lookupKind typeConName
    let currentType = mkType kind typeConName Nothing []
    pure $ foldr wrap currentType (toGQLWrapper typeWrappers)
    where
      wrap :: Monad m => DataTypeWrapper -> ResModel QUERY e m -> ResModel QUERY e m
      wrap wrapper contentType =
        mkObject
          "__Type"
          [ renderKind (wrapperKind wrapper),
            ("ofType", pure contentType)
          ]
      wrapperKind ListType = LIST
      wrapperKind NonNullType = NON_NULL

interfacePossibleTypes ::
  (Monad m) =>
  TypeName ->
  Resolver QUERY e m (ResModel QUERY e m)
interfacePossibleTypes interfaceName =
  mkList
    <$> ( getSchema
            >>= sequence
              . concatMap implements
              . elems
        )
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

mkType ::
  (Monad m, RenderIntrospection name) =>
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

createObjectType ::
  Monad m => TypeName -> Maybe Description -> [TypeName] -> FieldsDefinition OUT -> ResModel QUERY e m
createObjectType name desc interfaces fields =
  mkType OBJECT name desc [("fields", render fields), ("interfaces", mkList <$> traverse implementedInterface interfaces)]

implementedInterface ::
  (Monad m) =>
  TypeName ->
  Resolver QUERY e m (ResModel QUERY e m)
implementedInterface name =
  selectType name
    >>= renderContent
  where
    renderContent typeDef@TypeDefinition {typeContent = DataInterface {}} = render typeDef
    renderContent _ = failure ("Type " <> msg name <> " must be an Interface" :: Message)

opt :: Monad m => (a -> Resolver QUERY e m (ResModel QUERY e m)) -> Maybe a -> Resolver QUERY e m (ResModel QUERY e m)
opt f (Just x) = f x
opt _ Nothing = pure mkNull

renderName ::
  ( RenderIntrospection name,
    Monad m
  ) =>
  name ->
  (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderName = ("name",) . render

renderKind :: Monad m => TypeKind -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderKind = ("kind",) . render

type' :: Monad m => TypeRef -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
type' ref = ("type", render ref)

defaultValue ::
  Monad m =>
  TypeRef ->
  Maybe (Value CONST) ->
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
  Maybe (Value CONST) ->
  m (Value CONST)
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
  Object CONST ->
  FieldDefinition IN ->
  m (ObjectEntry CONST)
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
