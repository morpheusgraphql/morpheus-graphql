{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.RenderIntrospection
  ( render,
    createObjectType,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving.MonadResolver
  ( MonadResolver,
    ResolverContext (..),
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( ResolverValue,
    mkBoolean,
    mkList,
    mkNull,
    mkObject,
    mkString,
  )
import qualified Data.Morpheus.Core as GQL
import Data.Morpheus.Internal.Utils
  ( fromLBS,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition (..),
    ArgumentsDefinition,
    DataEnumValue (..),
    Description,
    DirectiveDefinition (..),
    DirectiveLocation,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    Msg (msg),
    OUT,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (BaseType, TypeList),
    UnionMember (..),
    VALID,
    Value (..),
    fieldVisibility,
    internal,
    kindOf,
    lookupDeprecated,
    lookupDeprecatedReason,
    mkInputUnionFields,
    msg,
    possibleInterfaceTypes,
    typeDefinitions,
    unpackName,
  )
import Data.Text (pack)
import Relude

selectType :: MonadResolver m => TypeName -> m (TypeDefinition ANY VALID)
selectType name =
  asks schema
    >>= selectBy (internal $ "INTROSPECTION Type not Found: \"" <> msg name <> "\"") name
      . typeDefinitions

class RenderIntrospection a where
  render :: (MonadResolver m) => a -> m (ResolverValue m)

instance RenderIntrospection TypeName where
  render = pure . mkString . unpackName

instance RenderIntrospection FieldName where
  render = pure . mkString . unpackName

instance RenderIntrospection Description where
  render = pure . mkString

instance RenderIntrospection a => RenderIntrospection [a] where
  render ls = mkList <$> traverse render ls

instance RenderIntrospection a => RenderIntrospection (Maybe a) where
  render (Just value) = render value
  render Nothing = pure mkNull

instance RenderIntrospection Bool where
  render = pure . mkBoolean

instance RenderIntrospection TypeKind where
  render = pure . mkString . fromLBS . GQL.render

instance RenderIntrospection (DirectiveDefinition VALID) where
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

instance RenderIntrospection (TypeDefinition cat VALID) where
  render
    TypeDefinition
      { typeName,
        typeDescription,
        typeContent
      } = pure $ renderContent typeContent
      where
        __type ::
          MonadResolver m =>
          TypeKind ->
          [(FieldName, m (ResolverValue m))] ->
          ResolverValue m
        __type kind = mkType kind typeName typeDescription
        renderContent :: MonadResolver m => TypeContent bool a VALID -> ResolverValue m
        renderContent DataScalar {} = __type KIND_SCALAR []
        renderContent (DataEnum enums) = __type KIND_ENUM [("enumValues", render enums)]
        renderContent (DataInputObject inputFields) =
          __type
            KIND_INPUT_OBJECT
            [("inputFields", render inputFields)]
        renderContent DataObject {objectImplements, objectFields} =
          createObjectType typeName typeDescription objectImplements objectFields
        renderContent (DataUnion union) =
          __type
            KIND_UNION
            [("possibleTypes", render $ toList union)]
        renderContent (DataInputUnion members) =
          mkType
            KIND_INPUT_OBJECT
            typeName
            ( Just
                ( "Note! This input is an exclusive object,\n"
                    <> "i.e., the customer can provide a value for only one field."
                )
                <> typeDescription
            )
            [ ( "inputFields",
                render (mkInputUnionFields members)
              )
            ]
        renderContent (DataInterface fields) =
          __type
            KIND_INTERFACE
            [ ("fields", render fields),
              ("possibleTypes", renderPossibleTypes typeName)
            ]

instance RenderIntrospection (UnionMember OUT s) where
  render UnionMember {memberName} = selectType memberName >>= render

instance
  RenderIntrospection (FieldDefinition cat s) =>
  RenderIntrospection (FieldsDefinition cat s)
  where
  render = render . filter fieldVisibility . toList

instance RenderIntrospection (FieldContent TRUE IN VALID) where
  render = render . defaultInputValue

instance RenderIntrospection (Value VALID) where
  render Null = pure mkNull
  render x = pure $ mkString $ fromLBS $ GQL.render x

instance RenderIntrospection (FieldDefinition OUT VALID) where
  render FieldDefinition {..} =
    pure $
      mkObject "__Field" $
        [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          ("args", maybe (pure $ mkList []) render fieldContent)
        ]
          <> renderDeprecated fieldDirectives

instance RenderIntrospection (FieldContent TRUE OUT VALID) where
  render (FieldArgs args) = render args

instance RenderIntrospection (ArgumentsDefinition VALID) where
  render = fmap mkList . traverse (render . argument) . toList

instance RenderIntrospection (FieldDefinition IN VALID) where
  render FieldDefinition {..} =
    pure $
      mkObject
        "__InputValue"
        [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          defaultValue fieldContent
        ]

instance RenderIntrospection (DataEnumValue VALID) where
  render DataEnumValue {enumName, enumDescription, enumDirectives} =
    pure $
      mkObject "__Field" $
        [ renderName enumName,
          description enumDescription
        ]
          <> renderDeprecated enumDirectives

instance RenderIntrospection TypeRef where
  render TypeRef {typeConName, typeWrappers} = renderWrapper typeWrappers
    where
      renderWrapper :: (Monad m, MonadResolver m) => TypeWrapper -> m (ResolverValue m)
      renderWrapper (TypeList nextWrapper isNonNull) =
        pure $
          withNonNull isNonNull $
            mkObject
              "__Type"
              [ renderKind KIND_LIST,
                ("ofType", renderWrapper nextWrapper)
              ]
      renderWrapper (BaseType isNonNull) =
        withNonNull isNonNull <$> do
          kind <- kindOf <$> selectType typeConName
          pure $ mkType kind typeConName Nothing []

withNonNull ::
  MonadResolver m =>
  Bool ->
  ResolverValue m ->
  ResolverValue m
withNonNull True contentType =
  mkObject
    "__Type"
    [ renderKind KIND_NON_NULL,
      ("ofType", pure contentType)
    ]
withNonNull False contentType = contentType

renderPossibleTypes ::
  MonadResolver m =>
  TypeName ->
  m (ResolverValue m)
renderPossibleTypes name = mkList <$> (asks schema >>= traverse render . possibleInterfaceTypes name)

renderDeprecated ::
  MonadResolver m =>
  Directives s ->
  [(FieldName, m (ResolverValue m))]
renderDeprecated dirs =
  [ ("isDeprecated", render (isJust $ lookupDeprecated dirs)),
    ("deprecationReason", render (lookupDeprecated dirs >>= lookupDeprecatedReason))
  ]

description ::
  MonadResolver m =>
  Maybe Description ->
  (FieldName, m (ResolverValue m))
description = ("description",) . render

mkType ::
  ( RenderIntrospection name,
    MonadResolver m
  ) =>
  TypeKind ->
  name ->
  Maybe Description ->
  [(FieldName, m (ResolverValue m))] ->
  ResolverValue m
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
  (MonadResolver m) =>
  TypeName ->
  Maybe Description ->
  [TypeName] ->
  FieldsDefinition OUT VALID ->
  ResolverValue m
createObjectType name desc interfaces fields =
  mkType (KIND_OBJECT Nothing) name desc [("fields", render fields), ("interfaces", mkList <$> traverse implementedInterface interfaces)]

implementedInterface ::
  (MonadResolver m) =>
  TypeName ->
  m (ResolverValue m)
implementedInterface name =
  selectType name
    >>= renderContent
  where
    renderContent typeDef@TypeDefinition {typeContent = DataInterface {}} = render typeDef
    renderContent _ = throwError $ internal $ "Type " <> msg name <> " must be an Interface"

renderName ::
  ( RenderIntrospection name,
    MonadResolver m
  ) =>
  name ->
  (FieldName, m (ResolverValue m))
renderName = ("name",) . render

renderKind ::
  MonadResolver m =>
  TypeKind ->
  (FieldName, m (ResolverValue m))
renderKind = ("kind",) . render

type' ::
  MonadResolver m =>
  TypeRef ->
  (FieldName, m (ResolverValue m))
type' = ("type",) . render

defaultValue ::
  MonadResolver m =>
  Maybe (FieldContent TRUE IN VALID) ->
  ( FieldName,
    m (ResolverValue m)
  )
defaultValue = ("defaultValue",) . render
