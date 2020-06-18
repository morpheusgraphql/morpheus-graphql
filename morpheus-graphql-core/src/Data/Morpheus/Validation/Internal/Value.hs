{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
  ( InputSource (..),
    InputValidator,
    Prop (..),
    askInputFieldType,
    askInputMember,
    askScopePosition,
    constraintInputUnion,
    inputMessagePrefix,
    inputValueSource,
    selectKnown,
    selectWithDefaultValue,
    withInputScope,
    withScopeType,
  )
import Data.Semigroup ((<>))

castFailure :: TypeRef -> Maybe Message -> ResolvedValue -> InputValidator a
castFailure expected message value = do
  pos <- askScopePosition
  prefix <- inputMessagePrefix
  failure
    $ errorMessage pos
    $ prefix <> typeViolation expected value <> maybe "" (" " <>) message

checkTypeEquality ::
  (TypeName, [TypeWrapper]) ->
  Ref ->
  Variable VALID ->
  InputValidator ValidValue
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

-- Validate input Values
validateInput ::
  [TypeWrapper] ->
  TypeDefinition IN ->
  ObjectEntry RESOLVED ->
  InputValidator ValidValue
validateInput tyWrappers TypeDefinition {typeContent = tyCont, typeName} =
  withScopeType typeName
    . validateWrapped tyWrappers tyCont
  where
    mismatchError :: [TypeWrapper] -> ResolvedValue -> InputValidator ValidValue
    mismatchError wrappers = castFailure (TypeRef typeName Nothing wrappers) Nothing
    -- VALIDATION
    validateWrapped ::
      [TypeWrapper] ->
      TypeContent TRUE IN ->
      ObjectEntry RESOLVED ->
      InputValidator ValidValue
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
    validateWrapped [] dt v = validate dt v
      where
        validate ::
          TypeContent TRUE IN -> ObjectEntry RESOLVED -> InputValidator ValidValue
        validate (DataInputObject parentFields) ObjectEntry {entryValue = Object fields} =
          Object
            <$> ( traverse_ (`requiredFieldIsDefined` fields) parentFields
                    *> traverse (`validateField` parentFields) fields
                )
        -- VALIDATE INPUT UNION
        validate (DataInputUnion inputUnion) ObjectEntry {entryValue = Object rawFields} =
          case constraintInputUnion inputUnion rawFields of
            Left message -> castFailure (TypeRef typeName Nothing []) (Just message) (Object rawFields)
            Right (name, Nothing) -> pure (mkInputObject name [])
            Right (name, Just value) -> validatInputUnionMember name value
        {-- VALIDATE ENUM --}
        validate (DataEnum tags) ObjectEntry {entryValue} =
          validateEnum (castFailure (TypeRef typeName Nothing []) Nothing) tags entryValue
        {-- VALIDATE SCALAR --}
        validate (DataScalar dataScalar) ObjectEntry {entryValue} =
          validateScalar typeName dataScalar entryValue (castFailure (TypeRef typeName Nothing []))
        validate _ ObjectEntry {entryValue} = mismatchError [] entryValue
    {-- 3. THROW ERROR: on invalid values --}
    validateWrapped wrappers _ ObjectEntry {entryValue} = mismatchError wrappers entryValue

-- INPUT UNION
validatInputUnionMember :: TypeName -> Value RESOLVED -> InputValidator (Value VALID)
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

-- INUT Fields
validateField ::
  ObjectEntry RESOLVED -> FieldsDefinition IN -> InputValidator (ObjectEntry VALID)
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

requiredFieldIsDefined :: FieldDefinition IN -> Object RESOLVED -> InputValidator (ObjectEntry RESOLVED)
requiredFieldIsDefined fieldDef@FieldDefinition {fieldName} =
  selectWithDefaultValue (ObjectEntry fieldName Null) fieldDef

-- Leaf Validations
validateScalar ::
  TypeName ->
  ScalarDefinition ->
  ResolvedValue ->
  (Maybe Message -> ResolvedValue -> InputValidator ValidValue) ->
  InputValidator ValidValue
validateScalar typeName ScalarDefinition {validateValue} value err = do
  scalarValue <- toScalar value
  case validateValue scalarValue of
    Right _ -> pure scalarValue
    Left "" -> err Nothing value
    Left message -> err (Just $ msg message) value
  where
    toScalar :: ResolvedValue -> InputValidator ValidValue
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

isVariableValue :: InputValidator Bool
isVariableValue =
  \case
    SourceVariable {isDefaultValue} -> not isDefaultValue
    _ -> False
    <$> inputValueSource

validateEnum ::
  (ResolvedValue -> InputValidator ValidValue) ->
  [DataEnumValue] ->
  ResolvedValue ->
  InputValidator ValidValue
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
