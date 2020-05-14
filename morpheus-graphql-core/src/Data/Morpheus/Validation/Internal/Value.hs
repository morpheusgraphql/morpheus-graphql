{-# LANGUAGE GADTs #-}
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
    IN,
    InputFieldsDefinition (..),
    Message,
    ObjectEntry (..),
    RESOLVED,
    Ref (..),
    ResolvedValue,
    ScalarDefinition (..),
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
  ( InputValidator,
    Prop (..),
    askInputFieldType,
    askInputMember,
    askScopePosition,
    constraintInputUnion,
    inputMessagePrefix,
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

-- Validate Variable Argument or all Possible input Values
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
      | isNullableWrapper wrappers = return Null
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
        validate (DataInputObject parentFields) ObjectEntry {entryValue = Object fields} = do
          traverse_ requiredFieldsDefined (unInputFieldsDefinition parentFields)
          Object <$> traverse validateField fields
          where
            requiredFieldsDefined :: FieldDefinition -> InputValidator (ObjectEntry RESOLVED)
            requiredFieldsDefined fieldDef@FieldDefinition {fieldName} =
              selectWithDefaultValue (ObjectEntry fieldName Null) fieldDef fields
            validateField ::
              ObjectEntry RESOLVED -> InputValidator (ObjectEntry VALID)
            validateField entry@ObjectEntry {entryName} = do
              inputField@FieldDefinition {fieldType = TypeRef {typeConName, typeWrappers}} <- getField
              inputTypeDef <- askInputFieldType inputField
              withInputScope (Prop entryName typeConName) $
                ObjectEntry entryName
                  <$> validateInput
                    typeWrappers
                    inputTypeDef
                    entry
              where
                getField = selectKnown entry parentFields
        -- VALIDATE INPUT UNION
        -- TODO: enhance input union Validation
        validate (DataInputUnion inputUnion) ObjectEntry {entryValue = Object rawFields} =
          case constraintInputUnion inputUnion rawFields of
            Left message -> castFailure (TypeRef typeName Nothing []) (Just message) (Object rawFields)
            Right (name, Nothing) -> return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name)])
            Right (name, Just value) -> do
              inputDef <- askInputMember name
              validValue <-
                validateInput
                  [TypeMaybe]
                  inputDef
                  (ObjectEntry (toFieldName name) value)
              return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name), ObjectEntry (toFieldName name) validValue])
        {-- VALIDATE ENUM --}
        validate (DataEnum tags) ObjectEntry {entryValue} =
          validateEnum (castFailure (TypeRef typeName Nothing []) Nothing) tags entryValue
        {-- VALIDATE SCALAR --}
        validate (DataScalar dataScalar) ObjectEntry {entryValue} =
          validateScalar dataScalar entryValue (castFailure (TypeRef typeName Nothing []))
        validate _ ObjectEntry {entryValue} = mismatchError [] entryValue
    {-- 3. THROW ERROR: on invalid values --}
    validateWrapped wrappers _ ObjectEntry {entryValue} = mismatchError wrappers entryValue

validateScalar ::
  ScalarDefinition ->
  ResolvedValue ->
  (Maybe Message -> ResolvedValue -> InputValidator ValidValue) ->
  InputValidator ValidValue
validateScalar ScalarDefinition {validateValue} value err = do
  scalarValue <- toScalar value
  case validateValue scalarValue of
    Right _ -> return scalarValue
    Left "" -> err Nothing value
    Left message -> err (Just $ msg message) value
  where
    toScalar :: ResolvedValue -> InputValidator ValidValue
    toScalar (Scalar x) = pure (Scalar x)
    toScalar scValue = err Nothing scValue

validateEnum ::
  (ResolvedValue -> InputValidator ValidValue) ->
  [DataEnumValue] ->
  ResolvedValue ->
  InputValidator ValidValue
validateEnum err enumValues value@(Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise = err value
  where
    tags = map enumName enumValues
validateEnum err _ value = err value
