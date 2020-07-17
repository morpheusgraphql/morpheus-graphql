{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Document.Validation
  ( ValidateSchema (..),
  )
where

import Control.Applicative ((*>), (<*>), Applicative (..), pure)
import Control.Monad ((>=>))
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe (..), maybe)
import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    PartialImplements (..),
  )
import Data.Morpheus.Internal.Utils
  ( (<:>),
    KeyOf (..),
    Selectable (..),
    elems,
    empty,
    failure,
    ordTraverse,
  )
import Data.Morpheus.Schema.Schema
  ( internalSchema,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition,
    ArgumentsDefinition (..),
    CONST,
    DataEnumValue (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    IN,
    OUT,
    Schema (..),
    Schema (..),
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    Typed (..),
    UnionMember (..),
    VALID,
    Value,
    isWeaker,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Types.Internal.Validation
  ( InputSource (..),
    InputValidator,
    Scope (..),
    ScopeKind (..),
    runValidator,
    selectType,
    startInput,
  )
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    Interface (..),
    SchemaValidator,
    TypeSystemContext (..),
    constraintInterface,
    inArgument,
    inField,
    inInterface,
    inType,
  )
import Data.Morpheus.Validation.Internal.Directive
  ( validateDirectives,
  )
import Data.Morpheus.Validation.Internal.Value
  ( validateInputByTypeRef,
  )
import Data.Proxy (Proxy (..))
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (&&),
    (.),
    (==),
    Bool,
    not,
    otherwise,
  )

class ValidateSchema s where
  validateSchema :: Bool -> Schema s -> Eventless (Schema VALID)

instance ValidateSchema CONST where
  validateSchema
    withSystem
    schema@Schema
      { types,
        query,
        mutation,
        subscription,
        directiveDefinitions
      } = do
      sysSchema <-
        if withSystem
          then internalSchema <:> schema
          else pure schema
      runValidator
        __validateSchema
        sysSchema
        Scope
          { position = Nothing,
            currentTypeName = "Root",
            currentTypeKind = KindObject Nothing,
            currentTypeWrappers = [],
            kind = TYPE,
            fieldname = "Root"
          }
        TypeSystemContext
          { local = ()
          }
      where
        __validateSchema :: SchemaValidator () (Schema VALID)
        __validateSchema =
          Schema
            <$> traverse validateType types
            <*> validateType query
            <*> validateOptional validateType mutation
            <*> validateOptional validateType subscription
            <*> traverse validateDirectiveDefinition directiveDefinitions

validateOptional :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
validateOptional f = maybe (pure Nothing) (fmap Just . f)

instance ValidateSchema VALID where
  validateSchema _ = pure

validateType ::
  TypeDefinition cat CONST ->
  SchemaValidator () (TypeDefinition cat VALID)
validateType
  TypeDefinition
    { typeName,
      typeFingerprint,
      typeDescription,
      typeDirectives,
      typeContent
    } =
    inType typeName $
      TypeDefinition
        typeName
        typeFingerprint
        typeDescription
        <$> validateDirectives (typeDirectiveLocation typeContent) typeDirectives
        <*> validateTypeContent typeContent

typeDirectiveLocation :: TypeContent a b c -> DirectiveLocation
typeDirectiveLocation DataObject {} = OBJECT
typeDirectiveLocation DataInputObject {} = INPUT_OBJECT
typeDirectiveLocation DataScalar {} = SCALAR
typeDirectiveLocation DataEnum {} = ENUM
typeDirectiveLocation DataInputUnion {} = OBJECT
typeDirectiveLocation DataUnion {} = UNION
typeDirectiveLocation DataInterface {} = INTERFACE

validateTypeContent ::
  TypeContent TRUE cat CONST ->
  SchemaValidator TypeName (TypeContent TRUE cat VALID)
validateTypeContent
  DataObject
    { objectImplements,
      objectFields
    } =
    do
      validateImplements objectImplements objectFields
      DataObject objectImplements <$> ordTraverse validateField objectFields
validateTypeContent DataInputObject {inputObjectFields} =
  DataInputObject <$> ordTraverse validateField inputObjectFields
validateTypeContent DataScalar {..} = pure DataScalar {..}
validateTypeContent DataEnum {enumMembers} = DataEnum <$> traverse validateEnumMember enumMembers
validateTypeContent DataInputUnion {inputUnionMembers} =
  DataInputUnion <$> traverse validateUnionMember inputUnionMembers
validateTypeContent DataUnion {unionMembers} = DataUnion <$> traverse validateUnionMember unionMembers
validateTypeContent (DataInterface fields) =
  DataInterface <$> ordTraverse validateField fields

validateEnumMember ::
  DataEnumValue CONST -> SchemaValidator TypeName (DataEnumValue VALID)
validateEnumMember DataEnumValue {enumDirectives = directives, ..} = do
  enumDirectives <- validateDirectives ENUM_VALUE directives
  pure DataEnumValue {..}

validateUnionMember ::
  UnionMember cat CONST -> SchemaValidator TypeName (UnionMember cat VALID)
validateUnionMember UnionMember {..} = pure UnionMember {..}

class FieldDirectiveLocation (cat :: TypeCategory) where
  directiveLocation :: Proxy cat -> DirectiveLocation

instance FieldDirectiveLocation OUT where
  directiveLocation _ = FIELD_DEFINITION

instance FieldDirectiveLocation IN where
  directiveLocation _ = INPUT_FIELD_DEFINITION

validateField ::
  forall cat.
  FieldDirectiveLocation cat =>
  FieldDefinition cat CONST ->
  SchemaValidator TypeName (FieldDefinition cat VALID)
validateField field@FieldDefinition {..} =
  inField
    fieldName
    ( FieldDefinition
        fieldName
        fieldDescription
        fieldType
        <$> validateDirectives (directiveLocation (Proxy @cat)) fieldDirectives
          <*> validateOptional (checkFieldContent field) fieldContent
    )

checkFieldContent ::
  FieldDefinition cat CONST ->
  FieldContent TRUE cat CONST ->
  SchemaValidator (TypeName, FieldName) (FieldContent TRUE cat VALID)
checkFieldContent _ (FieldArgs argsDef) = FieldArgs <$> validateArgumentsDefinition argsDef
checkFieldContent FieldDefinition {fieldType} (DefaultInputValue value) = do
  (typeName, fName) <- asks local
  DefaultInputValue
    <$> startInput
      (SourceInputField typeName fName Nothing)
      (validateDefaultValue fieldType value)

validateArgumentsDefinition ::
  ArgumentsDefinition CONST ->
  SchemaValidator (TypeName, FieldName) (ArgumentsDefinition VALID)
validateArgumentsDefinition (ArgumentsDefinition meta args) =
  ArgumentsDefinition meta
    <$> ordTraverse
      validateArgumentDefinition
      args

validateArgumentDefinition ::
  ArgumentDefinition CONST ->
  SchemaValidator (TypeName, FieldName) (ArgumentDefinition VALID)
validateArgumentDefinition FieldDefinition {..} =
  FieldDefinition
    fieldName
    fieldDescription
    fieldType
    <$> validateDirectives ARGUMENT_DEFINITION fieldDirectives
    <*> validateOptional (validateArgumentDefaultValue fieldName fieldType) fieldContent

validateArgumentDefaultValue ::
  FieldName ->
  TypeRef ->
  FieldContent TRUE IN CONST ->
  SchemaValidator (TypeName, FieldName) (FieldContent TRUE IN VALID)
validateArgumentDefaultValue argName fieldType (DefaultInputValue value) =
  do
    (typeName, fName) <- asks local
    v <-
      startInput
        (SourceInputField typeName fName (Just argName))
        (validateDefaultValue fieldType value)
    pure (DefaultInputValue v)

-- INETRFACE
----------------------------
validateImplements ::
  [TypeName] ->
  FieldsDefinition OUT CONST ->
  SchemaValidator TypeName ()
validateImplements objectImplements objectFields = do
  interface <- traverse selectInterface objectImplements
  traverse_ (mustBeSubset objectFields) interface

mustBeSubset ::
  FieldsDefinition OUT CONST ->
  (TypeName, FieldsDefinition OUT CONST) ->
  SchemaValidator TypeName ()
mustBeSubset objFields (typeName, fields) =
  inInterface typeName $
    traverse_ (checkInterfaceField objFields) (elems fields)

checkInterfaceField ::
  FieldsDefinition OUT CONST ->
  FieldDefinition OUT CONST ->
  SchemaValidator Interface ()
checkInterfaceField
  objFields
  interfaceField@FieldDefinition
    { fieldName,
      fieldDirectives
    } =
    inField fieldName $
      do
        _ <- validateDirectives FIELD_DEFINITION fieldDirectives
        selectOr err (isSuptype interfaceField) fieldName objFields
    where
      err = failImplements Missing

class PartialImplements ctx => TypeEq a ctx where
  isSuptype :: a -> a -> SchemaValidator ctx ()

instance TypeEq (FieldDefinition OUT CONST) (Interface, FieldName) where
  FieldDefinition
    { fieldType,
      fieldContent = args1
    }
    `isSuptype` FieldDefinition
      { fieldType = fieldType',
        fieldContent = args2
      } = (fieldType `isSuptype` fieldType') *> (args1 `isSuptype` args2)

instance TypeEq (Maybe (FieldContent TRUE OUT s)) (Interface, FieldName) where
  f1 `isSuptype` f2 = toARgs f1 `isSuptype` toARgs f2
    where
      toARgs :: Maybe (FieldContent TRUE OUT s) -> ArgumentsDefinition s
      toARgs (Just (FieldArgs args)) = args
      toARgs _ = empty

instance (PartialImplements ctx) => TypeEq TypeRef ctx where
  t1@TypeRef
    { typeConName,
      typeWrappers = w1
    }
    `isSuptype` t2@TypeRef
      { typeConName = name',
        typeWrappers = w2
      }
      | typeConName == name' && not (isWeaker w2 w1) = pure ()
      | otherwise =
        failImplements UnexpectedType {expectedType = t1, foundType = t2}

elemIn ::
  ( KeyOf k a,
    Selectable k a c,
    TypeEq a ctx
  ) =>
  a ->
  c ->
  SchemaValidator ctx ()
elemIn el = selectOr (failImplements Missing) (isSuptype el) (keyOf el)

instance TypeEq (ArgumentsDefinition s) (Interface, FieldName) where
  args1 `isSuptype` args2 = traverse_ validateArg (elems args1)
    where
      validateArg arg = inArgument (keyOf arg) $ elemIn arg args2

instance TypeEq (ArgumentDefinition s) (Interface, Field) where
  arg1 `isSuptype` arg2 = fieldType arg1 `isSuptype` fieldType arg2

-------------------------------
selectInterface ::
  TypeName ->
  SchemaValidator ctx (TypeName, FieldsDefinition OUT CONST)
selectInterface = selectType >=> constraintInterface

failImplements ::
  PartialImplements ctx =>
  ImplementsError ->
  SchemaValidator ctx a
failImplements err = do
  x <- asks local
  failure $ partialImplements x err

-- DEFAULT VALUE

validateDefaultValue ::
  TypeRef ->
  Value CONST ->
  InputValidator
    CONST
    (TypeSystemContext (TypeName, FieldName))
    (Value VALID)
validateDefaultValue typeRef =
  validateInputByTypeRef (Typed typeRef)

-- TODO: validate directives
validateDirectiveDefinition :: DirectiveDefinition CONST -> SchemaValidator () (DirectiveDefinition VALID)
validateDirectiveDefinition DirectiveDefinition {directiveDefinitionArgs = args, ..} =
  inType "Directive" $ inField directiveDefinitionName $ do
    directiveDefinitionArgs <- validateArgumentsDefinition args
    pure DirectiveDefinition {..}
