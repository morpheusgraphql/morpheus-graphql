{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Validation.Internal.Value (validateInput) where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (elem)
import Data.Maybe (maybe)
-- MORPHEUS

import Data.Morpheus.Error.Input (typeViolation)
import Data.Morpheus.Error.Utils (errorMessage)
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
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.Validation
  ( GetWith,
    InputContext,
    InputSource (..),
    InputValidator,
    MissingRequired,
    MonadContext,
    Prop (..),
    Scope (..),
    ScopeKind (..),
    SetWith,
    Unknown,
    Validator,
    askInputFieldType,
    askInputMember,
    asks,
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
  ( GetWith ctx Schema,
    GetWith ctx Scope
  ) =>
  TypeRef ->
  Maybe Message ->
  ResolvedValue ->
  InputValidator ctx a
castFailure expected message value = do
  pos <- asks position
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
  ( GetWith ctx Schema,
    GetWith ctx Scope,
    GetWith (InputContext ctx) InputSource,
    SetWith ctx Scope,
    MissingRequired (Object CONST) (InputContext ctx),
    Unknown (FieldsDefinition IN) (InputContext ctx)
  )

-- Validate input Values
validateInput ::
  forall ctx.
  ( InputConstraints ctx
  ) =>
  [TypeWrapper] ->
  TypeDefinition IN ->
  ObjectEntry CONST ->
  InputValidator ctx ValidValue
validateInput tyWrappers TypeDefinition {typeContent = tyCont, typeName} =
  withScopeType typeName
    . validateWrapped tyWrappers tyCont
  where
    mismatchError :: [TypeWrapper] -> Maybe Message -> ResolvedValue -> InputValidator ctx ValidValue
    mismatchError wrappers = castFailure (TypeRef typeName Nothing wrappers)
    -- VALIDATION
    validateWrapped ::
      [TypeWrapper] ->
      TypeContent TRUE IN ->
      ObjectEntry CONST ->
      InputValidator ctx ValidValue
    -- Validate Null. value = null ?
    validateWrapped wrappers _ ObjectEntry {entryValue = ResolvedVariable ref variable} =
      checkTypeEquality (typeName, wrappers) ref variable
    validateWrapped wrappers _ ObjectEntry {entryValue = Null}
      | isNullable wrappers = pure Null
      | otherwise = mismatchError wrappers Nothing Null
    -- Validate LIST
    validateWrapped [TypeMaybe] dt ObjectEntry {entryValue} =
      validateUnwrapped (mismatchError [TypeMaybe]) dt entryValue
    validateWrapped (TypeMaybe : wrappers) _ value =
      validateWrapped wrappers tyCont value
    validateWrapped (TypeList : wrappers) _ (ObjectEntry key (List list)) =
      List <$> traverse validateElement list
      where
        validateElement = validateWrapped wrappers tyCont . ObjectEntry key
    {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
    {-- VALIDATE OBJECT--}
    validateWrapped [] dt ObjectEntry {entryValue} =
      validateUnwrapped (mismatchError []) dt entryValue
    {-- 3. THROW ERROR: on invalid values --}
    validateWrapped wrappers _ ObjectEntry {entryValue} = mismatchError wrappers Nothing entryValue
    validateUnwrapped ::
      -- error
      (Maybe Message -> ResolvedValue -> InputValidator ctx ValidValue) ->
      TypeContent TRUE IN ->
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
  ( InputConstraints ctx
  ) =>
  TypeName ->
  DataInputUnion ->
  Object CONST ->
  InputValidator ctx (Value VALID)
validatInputUnion typeName inputUnion rawFields =
  case constraintInputUnion inputUnion rawFields of
    Left message -> castFailure (mkTypeRef typeName) (Just message) (Object rawFields)
    Right (name, Nothing) -> pure (mkInputObject name [])
    Right (name, Just value) -> validatInputUnionMember name value

validatInputUnionMember ::
  ( InputConstraints ctx
  ) =>
  TypeName ->
  Value CONST ->
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
  Object CONST ->
  InputValidator ctx (Object VALID)
validateInputObject fieldsDef object =
  do
    kind <- asks kind
    case kind of
      TYPE ->
        traverse_ (`requiredFieldIsDefined` object) fieldsDef
          *> traverse (`validateField` fieldsDef) object
      _ ->
        traverse_ (`selectKnown` fieldsDef) object
          *> validateObjectWithDefaultValue object fieldsDef

validateField ::
  ( InputConstraints ctx
  ) =>
  ObjectEntry CONST ->
  FieldsDefinition IN ->
  InputValidator ctx (ObjectEntry VALID)
validateField entry parentFields = do
  field <- selectKnown entry parentFields
  validateInputField field entry

validateObjectWithDefaultValue ::
  (InputConstraints c) =>
  Object CONST ->
  FieldsDefinition IN ->
  Validator (InputContext c) (Object VALID)
validateObjectWithDefaultValue object fieldsDef =
  traverse (validateFieldWithDefaultValue object) (elems fieldsDef)
    >>= fromElems

validateFieldWithDefaultValue ::
  (InputConstraints c) =>
  Object CONST ->
  FieldDefinition IN ->
  Validator (InputContext c) (ObjectEntry VALID)
validateFieldWithDefaultValue object fieldDef@FieldDefinition {fieldName} = do
  entry <- selectWithDefaultValue (ObjectEntry fieldName) fieldDef object
  validateInputField fieldDef entry

validateInputField ::
  (InputConstraints c) =>
  FieldDefinition IN ->
  ObjectEntry CONST ->
  Validator (InputContext c) (ObjectEntry VALID)
validateInputField fieldDef@FieldDefinition {fieldName, fieldType = TypeRef {typeConName, typeWrappers}} entry = do
  inputTypeDef <- askInputFieldType fieldDef
  withInputScope (Prop fieldName typeConName) $
    ObjectEntry fieldName
      <$> validateInput
        typeWrappers
        inputTypeDef
        entry

requiredFieldIsDefined ::
  ( MissingRequired (Object CONST) (InputContext ctx),
    GetWith ctx Scope
  ) =>
  FieldDefinition IN ->
  Object CONST ->
  InputValidator ctx (ObjectEntry CONST)
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
  [DataEnumValue] ->
  ResolvedValue ->
  m c ValidValue
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
