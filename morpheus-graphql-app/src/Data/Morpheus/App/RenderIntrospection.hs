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
  ( renderI,
  )
where

import Control.Monad.Except (MonadError (..))
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
import Data.Morpheus.Core (render)
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
    GQLError,
    IN,
    Msg (msg),
    Name,
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

iError :: GQLError -> GQLError
iError x = internal ("INTROSPECTION" <> x)

getType :: (MonadResolver m) => TypeName -> m TypeDef
getType name =
  asks schema
    >>= selectBy (iError $ "type \"" <> msg name <> "\" not found!") name
    . typeDefinitions

assertINTERFACE :: (MonadResolver m) => TypeDef -> m TypeDef
assertINTERFACE t@TypeDefinition {typeContent = DataInterface {}} = pure t
assertINTERFACE t = throwError $ iError $ "Type " <> msg (typeName t) <> " must be an Interface!"

type TypeDef = TypeDefinition ANY VALID

type IValue m = m (ResolverValue m)

type IField m = (FieldName, IValue m)

class RenderI a where
  renderI :: (MonadResolver m) => a -> IValue m

instance RenderI (Name t) where
  renderI = pure . mkString . unpackName

instance RenderI Description where
  renderI = pure . mkString

instance (RenderI a) => RenderI [a] where
  renderI ls = mkList <$> traverse renderI ls

instance (RenderI a) => RenderI (Maybe a) where
  renderI (Just value) = renderI value
  renderI Nothing = pure mkNull

instance RenderI Bool where
  renderI = pure . mkBoolean

instance RenderI TypeKind where
  renderI = pure . mkString . fromLBS . render

instance RenderI (DirectiveDefinition VALID) where
  renderI DirectiveDefinition {..} =
    object
      "__Directive"
      [ fName directiveDefinitionName,
        fDescription directiveDefinitionDescription,
        ("locations", renderI directiveDefinitionLocations),
        ("args", renderI directiveDefinitionArgs)
      ]

instance RenderI DirectiveLocation where
  renderI locations = pure $ mkString (pack $ show locations)

instance RenderI (TypeDefinition c VALID) where
  renderI TypeDefinition {..} = renderContent typeContent
    where
      __type :: (MonadResolver m) => TypeKind -> [IField m] -> IValue m
      __type kind = __Type kind typeName typeDescription
      renderContent :: (MonadResolver m) => TypeContent bool a VALID -> IValue m
      renderContent DataScalar {} = __type KIND_SCALAR []
      renderContent (DataEnum enums) = __type KIND_ENUM [("enumValues", renderI enums)]
      renderContent (DataInputObject inputFields) =
        __type
          KIND_INPUT_OBJECT
          [("inputFields", renderI inputFields)]
      renderContent DataObject {objectImplements, objectFields} =
        __Type
          (KIND_OBJECT Nothing)
          typeName
          typeDescription
          [ ("fields", renderI objectFields),
            ("interfaces", traverse (getType >=> assertINTERFACE) objectImplements >>= renderI)
          ]
      renderContent (DataUnion union) =
        __type
          KIND_UNION
          [("possibleTypes", renderI $ toList union)]
      renderContent (DataInputUnion members) =
        __Type
          KIND_INPUT_OBJECT
          typeName
          ( Just
              ( "Note! This input is an exclusive object,\n"
                  <> "i.e., the customer can provide a value for only one field."
              )
              <> typeDescription
          )
          [ ( "inputFields",
              renderI (mkInputUnionFields members)
            )
          ]
      renderContent (DataInterface fields) =
        __type
          KIND_INTERFACE
          [ ("fields", renderI fields),
            ("possibleTypes", asks schema >>= renderI . possibleInterfaceTypes typeName)
          ]

instance RenderI (UnionMember OUT s) where
  renderI UnionMember {memberName} = getType memberName >>= renderI

instance (RenderI (FieldDefinition cat s)) => RenderI (FieldsDefinition cat s) where
  renderI = renderI . filter fieldVisibility . toList

instance RenderI (FieldContent TRUE IN VALID) where
  renderI = renderI . defaultInputValue

instance RenderI (Value VALID) where
  renderI Null = pure mkNull
  renderI x = pure $ mkString $ fromLBS $ render x

instance RenderI (FieldDefinition OUT VALID) where
  renderI FieldDefinition {..} =
    object "__Field"
      $ [ fName fieldName,
          fDescription fieldDescription,
          fType fieldType,
          ("args", maybe (pure $ mkList []) renderI fieldContent)
        ]
      <> fDeprecated fieldDirectives

instance RenderI (FieldContent TRUE OUT VALID) where
  renderI (FieldArgs args) = renderI args

instance RenderI (ArgumentsDefinition VALID) where
  renderI = fmap mkList . traverse (renderI . argument) . toList

instance RenderI (FieldDefinition IN VALID) where
  renderI FieldDefinition {..} =
    object
      "__InputValue"
      [ fName fieldName,
        fDescription fieldDescription,
        fType fieldType,
        fDefaultValue fieldContent
      ]

instance RenderI (DataEnumValue VALID) where
  renderI DataEnumValue {..} =
    object "__EnumValue"
      $ [ fName enumName,
          fDescription enumDescription
        ]
      <> fDeprecated enumDirectives

instance RenderI TypeRef where
  renderI TypeRef {..} = renderWrapper typeWrappers
    where
      renderWrapper (TypeList nextWrapper isNonNull) =
        withNonNull isNonNull $ wrapper KIND_LIST (renderWrapper nextWrapper)
      renderWrapper (BaseType isNonNull) =
        withNonNull isNonNull $ do
          kind <- kindOf <$> getType typeConName
          __Type kind typeConName Nothing []

withNonNull :: (MonadResolver m) => Bool -> IValue m -> IValue m
withNonNull True = wrapper KIND_NON_NULL
withNonNull False = id

__Type ::
  (MonadResolver m) =>
  TypeKind ->
  Name t ->
  Maybe Description ->
  [IField m] ->
  IValue m
__Type kind name desc etc =
  object
    "__Type"
    ( [ fKind kind,
        fName name,
        fDescription desc
      ]
        <> etc
    )

wrapper :: (MonadResolver m) => TypeKind -> IValue m -> IValue m
wrapper k t =
  object
    "__Type"
    [ fKind k,
      ("ofType", t)
    ]

object :: (Monad m) => TypeName -> [IField m] -> IValue m
object name = pure . mkObject name

fDeprecated :: (MonadResolver m) => Directives s -> [IField m]
fDeprecated dirs =
  [ ("isDeprecated", renderI (isJust $ lookupDeprecated dirs)),
    ("deprecationReason", renderI (lookupDeprecated dirs >>= lookupDeprecatedReason))
  ]

fDescription :: (MonadResolver m) => Maybe Description -> IField m
fDescription = ("description",) . renderI

fName :: (MonadResolver m) => Name t -> IField m
fName = ("name",) . renderI

fKind :: (MonadResolver m) => TypeKind -> IField m
fKind = ("kind",) . renderI

fType :: (MonadResolver m) => TypeRef -> IField m
fType = ("type",) . renderI

fDefaultValue :: (MonadResolver m) => Maybe (FieldContent TRUE IN VALID) -> IField m
fDefaultValue = ("defaultValue",) . renderI
