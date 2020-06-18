{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Validation.Internal.Value (validateInput) where

import Data.Foldable (traverse_)
import Data.List (elem)
import Data.Maybe (maybe)
-- MORPHEUS

import Data.Morpheus.Error.Input (typeViolation)
import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Error.Variable (incompatibleVariableType)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( DataEnumValue (..),
    DataInputUnion,
    FieldDefinition (..),
    FieldsDefinition,
    IN,
    Message,
    Object,
    ObjectEntry (..),
    RESOLVED,
    Ref (..),
    ResolvedValue,
    ScalarDefinition (..),
    ScalarValue (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    TypeRef (..),
    TypeWrapper (..),
    VALID,
    ValidValue,
    Value (..),
    Variable (..),
    Variable (..),
    VariableContent (..),
    isNullableWrapper,
    isWeaker,
    msg,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.Validation
  ( InputContext,
    InputSource (..),
    InputValidator,
    MissingRequired,
    Prop (..),
    Unknown,
    Validator,
    WithInput,
    WithSchema,
    WithScope,
    askInputFieldType,
    askInputMember,
    askPosition,
    constraintInputUnion,
    inputMessagePrefix,
    inputValueSource,
    selectKnown,
    selectWithDefaultValue,
    withInputScope,
    withScopeType,
  )
import Data.Semigroup ((<>))

castFailure ::
  ( WithSchema (Validator (InputContext ctx)),
    WithScope (Validator (InputContext ctx))
  ) =>
  TypeRef ->
  Maybe Message ->
  ResolvedValue ->
  InputValidator ctx a
castFailure expected message value = do
  pos <- askPosition
  prefix <- inputMessagePrefix
  failure
    $ errorMessage pos
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

type InputConstraints ctx =
  ( WithSchema (Validator (InputContext ctx)),
    WithScope (Validator (InputContext ctx)),
    MissingRequired (Object RESOLVED) (InputContext ctx),
    Unknown (FieldsDefinition IN) (InputContext ctx),
    WithInput (Validator (InputContext ctx))
  )

-- Validate input Values
validateInput ::
  forall ctx.
  ( InputConstraints ctx
  ) =>
  [TypeWrapper] ->
  TypeDefinition IN ->
  ObjectEntry RESOLVED ->
  InputValidator ctx ValidValue
validateInput tyWrappers TypeDefinition {typeContent = tyCont, typeName} =
  withScopeType typeName
    . validateWrapped tyWrappers tyCont
  where
    mismatchError :: [TypeWrapper] -> ResolvedValue -> InputValidator ctx ValidValue
    mismatchError wrappers = castFailure (TypeRef typeName Nothing wrappers) Nothing
    -- VALIDATION
    validateWrapped ::
      [TypeWrapper] ->
      TypeContent TRUE IN ->
      ObjectEntry RESOLVED ->
      InputValidator ctx ValidValue
    -- Validate Null. value = null ?
    validateWrapped wrappers _ ObjectEntry {entryValue = ResolvedVariable ref variable} =
      checkTypeEquality (typeName, wrappers) ref variable
    validateWrapped wrappers _ ObjectEntry {entryValue = Null}
      | isNullableWrapper wrappers = pure Null
      | otherwise = mismatchError wrappers Null
    -- Validate LIST
    validateWrapped (TypeMaybe : wrappers) _ value =
      validateWrapped wrappers tyCont value
    validateWrapped (TypeList : wrappers) _ (ObjectEntry key (List list)) =
      List <$> traverse validateElement list
      where
        validateElement = validateWrapped wrappers tyCont . ObjectEntry key
    {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
    {-- VALIDATE OBJECT--}
    validateWrapped [] dt v = validateEntry dt v
      where
        validateEntry ::
          TypeContent TRUE IN ->
          ObjectEntry RESOLVED ->
          InputValidator ctx ValidValue
        validateEntry (DataInputObject parentFields) ObjectEntry {entryValue = Object fields} =
          Object <$> validateInputObject parentFields fields
        validateEntry (DataInputUnion inputUnion) ObjectEntry {entryValue = Object rawFields} =
          validatInputUnion typeName inputUnion rawFields
        validateEntry (DataEnum tags) ObjectEntry {entryValue} =
          validateEnum (castFailure (TypeRef typeName Nothing []) Nothing) tags entryValue
        validateEntry (DataScalar dataScalar) ObjectEntry {entryValue} =
          validateScalar typeName dataScalar entryValue (castFailure (TypeRef typeName Nothing []))
        validateEntry _ ObjectEntry {entryValue} = mismatchError [] entryValue
    {-- 3. THROW ERROR: on invalid values --}
    validateWrapped wrappers _ ObjectEntry {entryValue} = mismatchError wrappers entryValue

-- INPUT UNION
validatInputUnion ::
  ( InputConstraints ctx
  ) =>
  TypeName ->
  DataInputUnion ->
  Object RESOLVED ->
  InputValidator ctx (Value VALID)
validatInputUnion typeName inputUnion rawFields =
  case constraintInputUnion inputUnion rawFields of
    Left message -> castFailure (TypeRef typeName Nothing []) (Just message) (Object rawFields)
    Right (name, Nothing) -> pure (mkInputObject name [])
    Right (name, Just value) -> validatInputUnionMember name value

validatInputUnionMember ::
  ( InputConstraints ctx
  ) =>
  TypeName ->
  Value RESOLVED ->
  InputValidator ctx (Value VALID)
validatInputUnionMember name value = do
  inputDef <- askInputMember name
  validValue <-
    validateInput
      [TypeMaybe]
      inputDef
      (ObjectEntry (toFieldName name) value)
  pure $ mkInputObject name [ObjectEntry (toFieldName name) validValue]

mkInputObject :: TypeName -> [ObjectEntry s] -> Value s
mkInputObject name xs = Object $ unsafeFromValues $ ObjectEntry "__typename" (Enum name) : xs

-- INUT Object
validateInputObject ::
  ( InputConstraints ctx
  ) =>
  FieldsDefinition IN ->
  Object RESOLVED ->
  InputValidator ctx (Object VALID)
validateInputObject parentFields fields =
  traverse_ (`requiredFieldIsDefined` fields) parentFields
    *> traverse (`validateField` parentFields) fields

validateField ::
  ( InputConstraints ctx
  ) =>
  ObjectEntry RESOLVED ->
  FieldsDefinition IN ->
  InputValidator ctx (ObjectEntry VALID)
validateField entry@ObjectEntry {entryName} parentFields = do
  field@FieldDefinition {fieldType = TypeRef {typeConName, typeWrappers}} <-
    selectKnown entry parentFields
  inputTypeDef <- askInputFieldType field
  withInputScope (Prop entryName typeConName) $
    ObjectEntry entryName
      <$> validateInput
        typeWrappers
        inputTypeDef
        entry

requiredFieldIsDefined ::
  ( MissingRequired (Object RESOLVED) (InputContext ctx),
    WithScope (Validator (InputContext ctx))
  ) =>
  FieldDefinition IN ->
  Object RESOLVED ->
  InputValidator ctx (ObjectEntry RESOLVED)
requiredFieldIsDefined fieldDef@FieldDefinition {fieldName} =
  selectWithDefaultValue (ObjectEntry fieldName) fieldDef

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
    isValidDefault "Float" = \x -> isFloat x || isInt x
    isValidDefault "Int" = isInt
    isValidDefault _ = const True

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

isVariableValue :: WithInput m => m Bool
isVariableValue =
  \case
    SourceVariable {isDefaultValue} -> not isDefaultValue
    _ -> False
    <$> inputValueSource

validateEnum ::
  (Monad m, WithInput m) =>
  (ResolvedValue -> m ValidValue) ->
  [DataEnumValue] ->
  ResolvedValue ->
  m ValidValue
validateEnum err enumValues value@(Scalar (String enumValue))
  | TypeName enumValue `elem` tags = do
    isFromVariable <- isVariableValue
    if isFromVariable
      then pure (Enum (TypeName enumValue))
      else err value
  where
    tags = map enumName enumValues
validateEnum err enumValues value@(Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise = err value
  where
    tags = map enumName enumValues
validateEnum err _ value = err value
