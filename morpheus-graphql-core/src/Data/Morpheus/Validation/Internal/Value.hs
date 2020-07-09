{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Value
  ( Validate,
    ValueContext,
    validateInputByField,
    validateInputByTypeRef,
    validateInputByType,
  )
where

import Control.Applicative ((*>), pure)
import Control.Monad (Monad ((>>=)))
import Data.Either (Either (..))
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<$>), Functor, fmap)
import Data.List (any, elem)
import Data.Maybe (Maybe (..), maybe)
import Data.Morpheus.Error.Input (typeViolation)
import Data.Morpheus.Error.Utils (renderErrorMessage)
import Data.Morpheus.Error.Variable (incompatibleVariableType)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
    fromElems,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataEnumValue (..),
    DataInputUnion,
    FieldDefinition (..),
    FieldsDefinition,
    IN,
    Message,
    Object,
    ObjectEntry (..),
    Ref (..),
    ResolvedValue,
    ScalarDefinition (..),
    ScalarValue (..),
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    VALID,
    ValidValue,
    Value (..),
    Variable (..),
    Variable (..),
    VariableContent (..),
    isNullable,
    isWeaker,
    mkTypeRef,
    msg,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.AST.OrdMap
  ( unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.Validation
  ( GetWith,
    InputContext,
    InputSource (..),
    InputValidator,
    MonadContext,
    Prop (..),
    Scope (..),
    ScopeKind (..),
    Validate (..),
    Validator,
    askInputFieldType,
    askInputFieldTypeByName,
    askInputMember,
    asksScope,
    constraintInputUnion,
    inputMessagePrefix,
    inputValueSource,
    selectKnown,
    selectWithDefaultValue,
    withInputScope,
    withScopeType,
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (&&),
    (.),
    Bool (..),
    Eq (..),
    const,
    not,
    otherwise,
  )

castFailure ::
  TypeRef ->
  Maybe Message ->
  Value s ->
  InputValidator ctx a
castFailure expected message value = do
  pos <- asksScope position
  prefix <- inputMessagePrefix
  failure
    $ renderErrorMessage pos
    $ prefix <> typeViolation expected value <> maybe "" (" " <>) message

checkTypeEquality ::
  (TypeName, [TypeWrapper]) ->
  Ref ->
  Variable VALID ->
  InputValidator ctx ValidValue
checkTypeEquality (tyConName, tyWrappers) ref var@Variable {variableValue = ValidVariableValue value, variableType}
  | typeConName variableType == tyConName
      && not
        (isWeaker (typeWrappers variableType) tyWrappers) =
    pure value
  | otherwise =
    failure $
      incompatibleVariableType
        ref
        var
        TypeRef
          { typeConName = tyConName,
            typeWrappers = tyWrappers,
            typeArgs = Nothing
          }

type ValueConstraints ctx schemaS s =
  ( GetWith ctx (Schema schemaS),
    Validate (ValueContext schemaS) Value s (InputContext ctx)
  )

type InputConstraints ctx schemaS s =
  ( ValueConstraints ctx schemaS s,
    ValidateWithDefault ctx schemaS s
  )

validateInputByType ::
  ValueConstraints c schemaS s =>
  [TypeWrapper] ->
  TypeDefinition IN schemaS ->
  Value s ->
  Validator (InputContext c) (Value VALID)
validateInputByType typeWrappers inputTypeDef =
  validate
    ( ValueContext
        typeWrappers
        inputTypeDef
    )

validateInputByTypeRef ::
  forall schemaS s c.
  ValueConstraints c schemaS s =>
  Proxy schemaS ->
  TypeRef ->
  Value s ->
  Validator (InputContext c) (Value VALID)
validateInputByTypeRef
  _
  TypeRef {typeWrappers, typeConName}
  value = do
    (inputTypeDef :: TypeDefinition IN schemaS) <- askInputFieldTypeByName typeConName
    validateInputByType typeWrappers inputTypeDef value

validateInputByField ::
  ValueConstraints c schemaS s =>
  FieldDefinition IN schemaS ->
  Value s ->
  Validator (InputContext c) (Value VALID)
validateInputByField
  fieldDef@FieldDefinition
    { fieldType = TypeRef {typeWrappers}
    }
  value = do
    inputTypeDef <- askInputFieldType fieldDef
    validateInputByType typeWrappers inputTypeDef value

validateValueByField ::
  ValueConstraints c schemaS s =>
  FieldDefinition IN schemaS ->
  Value s ->
  Validator (InputContext c) (Value VALID)
validateValueByField
  fieldDef@FieldDefinition
    { fieldName,
      fieldType = TypeRef {typeConName}
    } =
    withInputScope (Prop fieldName typeConName) . validateInputByField fieldDef

instance
  InputConstraints ctx VALID CONST =>
  Validate (ValueContext VALID) Value CONST (InputContext ctx)
  where
  validate (ValueContext x y) = validateInput x y

instance
  InputConstraints ctx CONST CONST =>
  Validate (ValueContext CONST) Value CONST (InputContext ctx)
  where
  validate (ValueContext x y) = validateInput x y

data ValueContext s = ValueContext
  { valueWrappers :: [TypeWrapper],
    valueTypeDef :: TypeDefinition IN s
  }

-- Validate input Values
validateInput ::
  forall ctx s.
  ( InputConstraints ctx s CONST
  ) =>
  [TypeWrapper] ->
  TypeDefinition IN s ->
  Value CONST ->
  InputValidator ctx ValidValue
validateInput tyWrappers TypeDefinition {typeContent = tyCont, typeName} =
  withScopeType typeName
    . validateWrapped tyWrappers tyCont
  where
    mismatchError :: [TypeWrapper] -> Maybe Message -> Value CONST -> InputValidator ctx (Value VALID)
    mismatchError wrappers = castFailure (TypeRef typeName Nothing wrappers)
    -- VALIDATION
    validateWrapped ::
      [TypeWrapper] ->
      TypeContent TRUE IN s ->
      Value CONST ->
      InputValidator ctx ValidValue
    -- Validate Null. value = null ?
    validateWrapped wrappers _ (ResolvedVariable ref variable) =
      checkTypeEquality (typeName, wrappers) ref variable
    validateWrapped wrappers _ Null
      | isNullable wrappers = pure Null
      | otherwise = mismatchError wrappers Nothing Null
    -- Validate LIST
    validateWrapped [TypeMaybe] dt entryValue =
      validateUnwrapped (mismatchError [TypeMaybe]) dt entryValue
    validateWrapped (TypeMaybe : wrappers) _ value =
      validateWrapped wrappers tyCont value
    validateWrapped (TypeList : wrappers) _ (List list) =
      List <$> traverse validateElement list
      where
        validateElement = validateWrapped wrappers tyCont
    {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
    {-- VALIDATE OBJECT--}
    validateWrapped [] dt entryValue =
      validateUnwrapped (mismatchError []) dt entryValue
    {-- 3. THROW ERROR: on invalid values --}
    validateWrapped wrappers _ entryValue = mismatchError wrappers Nothing entryValue
    validateUnwrapped ::
      -- error
      (Maybe Message -> ResolvedValue -> InputValidator ctx ValidValue) ->
      TypeContent TRUE IN s ->
      Value CONST ->
      InputValidator ctx ValidValue
    validateUnwrapped _ (DataInputObject parentFields) (Object fields) =
      Object <$> validateInputObject parentFields fields
    validateUnwrapped _ (DataInputUnion inputUnion) (Object rawFields) =
      validatInputUnion typeName inputUnion rawFields
    validateUnwrapped err (DataEnum tags) value =
      validateEnum (err Nothing) tags value
    validateUnwrapped err (DataScalar dataScalar) value =
      validateScalar typeName dataScalar value err
    validateUnwrapped err _ value = err Nothing value

-- INPUT UNION
validatInputUnion ::
  forall schemaS s ctx.
  ValueConstraints ctx schemaS s =>
  TypeName ->
  DataInputUnion schemaS ->
  Object s ->
  InputValidator ctx (Value VALID)
validatInputUnion typeName inputUnion rawFields =
  case constraintInputUnion inputUnion rawFields of
    Left message -> castFailure (mkTypeRef typeName) (Just message) (Object rawFields)
    Right (name, Nothing) -> pure (mkInputObject name [])
    Right (name, Just value) -> validatInputUnionMember f name value
      where
        f :: TypeDefinition IN schemaS -> ValueContext schemaS
        f = ValueContext [TypeMaybe]

validatInputUnionMember ::
  ValueConstraints ctx schemaS s =>
  (TypeDefinition IN schemaS -> ValueContext schemaS) ->
  TypeName ->
  Value s ->
  InputValidator ctx (Value VALID)
validatInputUnionMember f name value = do
  inputDef <- askInputMember name
  validValue <- validate (f inputDef) value
  pure $ mkInputObject name [ObjectEntry (toFieldName name) validValue]

mkInputObject :: TypeName -> [ObjectEntry s] -> Value s
mkInputObject name xs = Object $ unsafeFromValues $ ObjectEntry "__typename" (Enum name) : xs

-- INUT Object
validateInputObject ::
  InputConstraints ctx s CONST =>
  FieldsDefinition IN s ->
  Object CONST ->
  InputValidator ctx (Object VALID)
validateInputObject fieldsDef object =
  do
    kind <- asksScope kind
    case kind of
      TYPE ->
        traverse_ (`requiredFieldIsDefined` object) fieldsDef
          *> traverse (validateField fieldsDef) object
      _ ->
        traverse_ (`selectKnown` fieldsDef) object
          *> validateObjectWithDefaultValue fieldsDef object

validateField ::
  ValueConstraints ctx s CONST =>
  FieldsDefinition IN s ->
  ObjectEntry CONST ->
  InputValidator ctx (ObjectEntry VALID)
validateField parentFields entry = do
  field <- selectKnown entry parentFields
  withEntry (validateValueByField field) entry

validateObjectWithDefaultValue ::
  ValidateWithDefault c s CONST =>
  FieldsDefinition IN s ->
  Object CONST ->
  Validator (InputContext c) (Object VALID)
validateObjectWithDefaultValue fieldsDef object =
  traverse (validateWithDefault object) (elems fieldsDef)
    >>= fromElems

class ValidateWithDefault c schemaS s where
  validateWithDefault ::
    Object s ->
    FieldDefinition IN schemaS ->
    Validator (InputContext c) (ObjectEntry VALID)

instance
  ValueConstraints c VALID s =>
  ValidateWithDefault c VALID s
  where
  validateWithDefault object fieldDef@FieldDefinition {fieldName} =
    ObjectEntry fieldName
      <$> selectWithDefaultValue
        pure
        (validateValueByField fieldDef . entryValue)
        fieldDef
        object

instance
  ValueConstraints c CONST s =>
  ValidateWithDefault c CONST s
  where
  validateWithDefault object fieldDef@FieldDefinition {fieldName} =
    ObjectEntry fieldName
      <$> selectWithDefaultValue
        (validateValueByField fieldDef)
        (validateValueByField fieldDef . entryValue)
        fieldDef
        object

withEntry ::
  Functor f =>
  (Value a -> f (Value b)) ->
  ObjectEntry a ->
  f (ObjectEntry b)
withEntry f ObjectEntry {entryName, entryValue} =
  ObjectEntry entryName <$> f entryValue

requiredFieldIsDefined ::
  FieldDefinition IN s ->
  Object CONST ->
  InputValidator ctx ()
requiredFieldIsDefined = selectWithDefaultValue (const $ pure ()) (const $ pure ())

-- Leaf Validations
validateScalar ::
  forall m.
  (Monad m) =>
  TypeName ->
  ScalarDefinition ->
  ResolvedValue ->
  (Maybe Message -> ResolvedValue -> m ValidValue) ->
  m ValidValue
validateScalar typeName ScalarDefinition {validateValue} value err = do
  scalarValue <- toScalar value
  case validateValue scalarValue of
    Right _ -> pure scalarValue
    Left "" -> err Nothing value
    Left message -> err (Just $ msg message) value
  where
    toScalar :: ResolvedValue -> m ValidValue
    toScalar (Scalar x) | isValidDefault typeName x = pure (Scalar x)
    toScalar _ = err Nothing value
    isValidDefault :: TypeName -> ScalarValue -> Bool
    isValidDefault "Boolean" = isBoolean
    isValidDefault "String" = isString
    isValidDefault "Float" = oneOf [isFloat, isInt]
    isValidDefault "Int" = isInt
    isValidDefault "ID" = oneOf [isInt, isFloat, isString]
    isValidDefault _ = const True

oneOf :: [a -> Bool] -> a -> Bool
oneOf ls v = any (v &) ls

isBoolean :: ScalarValue -> Bool
isBoolean Boolean {} = True
isBoolean _ = False

isString :: ScalarValue -> Bool
isString String {} = True
isString _ = False

isFloat :: ScalarValue -> Bool
isFloat Float {} = True
isFloat _ = False

isInt :: ScalarValue -> Bool
isInt Int {} = True
isInt _ = False

isVariableValue :: (MonadContext m c, GetWith c InputSource) => m c Bool
isVariableValue =
  \case
    SourceVariable {isDefaultValue} -> not isDefaultValue
    _ -> False
    <$> inputValueSource

validateEnum ::
  (MonadContext m c, GetWith c InputSource) =>
  (ResolvedValue -> m c ValidValue) ->
  [DataEnumValue s] ->
  ResolvedValue ->
  m c ValidValue
validateEnum err enumValues value@(Scalar (String enumValue))
  | TypeName enumValue `elem` tags = do
    isFromVariable <- isVariableValue
    if isFromVariable
      then pure (Enum (TypeName enumValue))
      else err value
  where
    tags = fmap enumName enumValues
validateEnum err enumValues value@(Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise = err value
  where
    tags = fmap enumName enumValues
validateEnum err _ value = err value
