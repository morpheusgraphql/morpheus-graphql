{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInputByTypeRef,
    validateInputByType,
    ValidateWithDefault,
  )
where

import Control.Applicative ((*>), pure)
import Data.Either (Either (..))
import Data.Function ((&))
import Data.Functor ((<$>), fmap)
import Data.List (any, elem)
import Data.Maybe (Maybe (..), maybe)
import Data.Morpheus.Error.Input (typeViolation)
import Data.Morpheus.Error.Utils (renderErrorMessage)
import Data.Morpheus.Error.Variable (incompatibleVariableType)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    ordTraverse_,
    traverseCollection,
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
    ScalarDefinition (..),
    ScalarValue (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    Typed (..),
    UnionMember (..),
    VALID,
    ValidValue,
    Value (..),
    Variable (..),
    Variable (..),
    VariableContent (..),
    isNullable,
    isWeaker,
    msg,
    toFieldName,
    typed,
    untyped,
  )
import Data.Morpheus.Types.Internal.AST.OrdMap
  ( unsafeFromValues,
  )
import Data.Morpheus.Types.Internal.Validation
  ( InputContext,
    InputSource (..),
    InputValidator,
    Scope (..),
    Validator,
    askType,
    askTypeMember,
    asksScope,
    constraintInputUnion,
    inField,
    inputMessagePrefix,
    inputValueSource,
    selectKnown,
    selectWithDefaultValue,
    withScopeType,
  )
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (&&),
    (.),
    Bool (..),
    Eq (..),
    const,
    fst,
    id,
    not,
    otherwise,
  )

castFailure ::
  [TypeWrapper] ->
  Maybe Message ->
  Value s ->
  InputValidator schemaS ctx a
castFailure wrappers message value = do
  Scope {position, currentTypeName} <- asksScope id
  prefix <- inputMessagePrefix
  failure
    $ renderErrorMessage position
    $ prefix
      <> typeViolation
        (TypeRef currentTypeName Nothing wrappers)
        value
      <> maybe "" (" " <>) message

unwrapppedFailure ::
  Bool ->
  Maybe Message ->
  Value s ->
  InputValidator schemaS ctx a
unwrapppedFailure nullable
  | nullable = castFailure [TypeMaybe]
  | otherwise = castFailure []

checkTypeEquality ::
  (TypeName, [TypeWrapper]) ->
  Ref ->
  Variable VALID ->
  InputValidator schemaS ctx ValidValue
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

validateInputByTypeRef ::
  ValidateWithDefault c schemaS s =>
  Typed IN schemaS TypeRef ->
  Value s ->
  Validator schemaS (InputContext c) (Value VALID)
validateInputByTypeRef
  ref
  value = do
    inputTypeDef <- askType ref
    validateInputByType
      (untyped typeWrappers ref)
      inputTypeDef
      value

validateValueByField ::
  ValidateWithDefault c schemaS s =>
  FieldDefinition IN schemaS ->
  Value s ->
  Validator schemaS (InputContext c) (Value VALID)
validateValueByField field =
  inField field
    . validateInputByTypeRef
      (typed fieldType field)

-- Validate input Values
validateInputByType ::
  ( ValidateWithDefault ctx schemaS valueS
  ) =>
  [TypeWrapper] ->
  TypeDefinition IN schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
validateInputByType tyWrappers typeDef@TypeDefinition {typeContent = tyCont} =
  withScopeType typeDef . validateWrapped tyWrappers tyCont

-- VALIDATION
validateWrapped ::
  ValidateWithDefault ctx schemaS valueS =>
  [TypeWrapper] ->
  TypeContent TRUE IN schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
-- Validate Null. value = null ?
validateWrapped wrappers _ (ResolvedVariable ref variable) = do
  typeName <- asksScope currentTypeName
  checkTypeEquality (typeName, wrappers) ref variable
validateWrapped wrappers _ Null
  | isNullable wrappers = pure Null
  | otherwise = castFailure wrappers Nothing Null
-- Validate LIST
validateWrapped [TypeMaybe] dt entryValue = validateUnwrapped True dt entryValue
validateWrapped (TypeMaybe : wrappers) tyCont value =
  validateWrapped wrappers tyCont value
validateWrapped (TypeList : wrappers) tyCont (List list) =
  List <$> traverse (validateWrapped wrappers tyCont) list
{-- 2. VALIDATE TYPES, all wrappers are already Processed --}
{-- VALIDATE OBJECT--}
validateWrapped [] dt entryValue = validateUnwrapped False dt entryValue
{-- 3. THROW ERROR: on invalid values --}
validateWrapped wrappers _ entryValue = castFailure wrappers Nothing entryValue

validateUnwrapped ::
  ValidateWithDefault ctx schemaS valueS =>
  Bool ->
  TypeContent TRUE IN schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
validateUnwrapped _ (DataInputObject parentFields) (Object fields) =
  Object <$> validateInputObject parentFields fields
validateUnwrapped _ (DataInputUnion inputUnion) (Object rawFields) =
  validatInputUnion inputUnion rawFields
validateUnwrapped nullable (DataEnum tags) value =
  validateEnum nullable tags value
validateUnwrapped nullable (DataScalar dataScalar) value =
  validateScalar nullable dataScalar value
validateUnwrapped nullable _ value = unwrapppedFailure nullable Nothing value

-- INPUT UNION
validatInputUnion ::
  ValidateWithDefault ctx schemaS s =>
  DataInputUnion schemaS ->
  Object s ->
  InputValidator schemaS ctx (Value VALID)
validatInputUnion inputUnion rawFields =
  case constraintInputUnion inputUnion rawFields of
    Left message -> castFailure [] (Just message) (Object rawFields)
    Right (UnionMember {memberName}, Nothing) -> pure (mkInputObject memberName [])
    Right (name, Just value) -> validatInputUnionMember name value

validatInputUnionMember ::
  ValidateWithDefault ctx schemaS valueS =>
  UnionMember IN schemaS ->
  Value valueS ->
  InputValidator schemaS ctx (Value VALID)
validatInputUnionMember member@UnionMember {memberName} value = do
  inputDef <- fst <$> askTypeMember member
  validValue <- validateInputByType [TypeMaybe] inputDef value
  pure $ mkInputObject memberName [ObjectEntry (toFieldName memberName) validValue]

mkInputObject :: TypeName -> [ObjectEntry s] -> Value s
mkInputObject name xs = Object $ unsafeFromValues $ ObjectEntry "__typename" (Enum name) : xs

-- INUT Object
validateInputObject ::
  ValidateWithDefault ctx schemaS valueS =>
  FieldsDefinition IN schemaS ->
  Object valueS ->
  InputValidator schemaS ctx (Object VALID)
validateInputObject fieldsDef object =
  ordTraverse_ (`selectKnown` fieldsDef) object
    *> traverseCollection (validateWithDefault object) fieldsDef

class ValidateWithDefault c schemaS s where
  validateWithDefault ::
    Object s ->
    FieldDefinition IN schemaS ->
    Validator schemaS (InputContext c) (ObjectEntry VALID)

instance ValidateWithDefault c VALID s where
  validateWithDefault object fieldDef@FieldDefinition {fieldName} =
    ObjectEntry fieldName
      <$> selectWithDefaultValue
        pure
        (validateValueByField fieldDef . entryValue)
        fieldDef
        object

instance ValidateWithDefault c CONST s where
  validateWithDefault object fieldDef@FieldDefinition {fieldName} =
    ObjectEntry fieldName
      <$> selectWithDefaultValue
        (validateValueByField fieldDef)
        (validateValueByField fieldDef . entryValue)
        fieldDef
        object

-- Leaf Validations
validateScalar ::
  Bool ->
  ScalarDefinition ->
  Value s ->
  InputValidator schemaS ctx ValidValue
validateScalar nullable ScalarDefinition {validateValue} value = do
  typeName <- asksScope currentTypeName
  scalarValue <- toScalar typeName value
  case validateValue scalarValue of
    Right _ -> pure scalarValue
    Left "" -> unwrapppedFailure nullable Nothing value
    Left message -> unwrapppedFailure nullable (Just $ msg message) value
  where
    toScalar :: TypeName -> Value s -> InputValidator schemaS ctx ValidValue
    toScalar typeName (Scalar x) | isValidDefault typeName x = pure (Scalar x)
    toScalar _ _ = unwrapppedFailure nullable Nothing value

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

isVariableValue :: InputValidator schemaS c Bool
isVariableValue =
  \case
    SourceVariable {isDefaultValue} -> not isDefaultValue
    _ -> False
    <$> inputValueSource

validateEnum ::
  Bool ->
  [DataEnumValue s] ->
  Value valueS ->
  InputValidator schemaS c ValidValue
validateEnum nullable enumValues value@(Scalar (String enumValue))
  | TypeName enumValue `elem` tags = do
    isFromVariable <- isVariableValue
    if isFromVariable
      then pure (Enum (TypeName enumValue))
      else unwrapppedFailure nullable Nothing value
  where
    tags = fmap enumName enumValues
validateEnum nullable enumValues value@(Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise = unwrapppedFailure nullable Nothing value
  where
    tags = fmap enumName enumValues
validateEnum nullable _ value = unwrapppedFailure nullable Nothing value
