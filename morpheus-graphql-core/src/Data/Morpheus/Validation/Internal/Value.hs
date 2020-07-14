{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInputByTypeRef,
    validateInputByType,
    ValueConstraints,
    ValidateWithDefault,
  )
where

import Control.Applicative ((*>), pure)
import Control.Monad (Monad)
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
    Schema,
    Stage,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    TypedRef (..),
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
    Validator,
    askInputMember,
    askTypeByRef,
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

type ValueConstraints (ctx :: *) (schemaS :: Stage) (s :: Stage) =
  ( GetWith ctx (Schema schemaS),
    ValidateWithDefault ctx schemaS s
  )

validateInputByType ::
  (ValueConstraints c schemaS s) =>
  [TypeWrapper] ->
  TypeDefinition IN schemaS ->
  Value s ->
  Validator (InputContext c) (Value VALID)
validateInputByType = validateInput

validateInputByTypeRef ::
  ValueConstraints c schemaS s =>
  TypedRef IN schemaS ->
  Value s ->
  Validator (InputContext c) (Value VALID)
validateInputByTypeRef
  ref@(TypedRef TypeRef {typeWrappers})
  value = do
    inputTypeDef <- askTypeByRef ref
    validateInputByType typeWrappers inputTypeDef value

validateValueByField ::
  forall schemaS s c.
  ValueConstraints c schemaS s =>
  FieldDefinition IN schemaS ->
  Value s ->
  Validator (InputContext c) (Value VALID)
validateValueByField
  FieldDefinition
    { fieldName,
      fieldType = typeRef@TypeRef {typeConName}
    } =
    withInputScope (Prop fieldName typeConName)
      . validateInputByTypeRef
        (TypedRef typeRef :: TypedRef IN schemaS)

-- Validate input Values
validateInput ::
  forall ctx schemaS valueS.
  ( ValueConstraints ctx schemaS valueS
  ) =>
  [TypeWrapper] ->
  TypeDefinition IN schemaS ->
  Value valueS ->
  InputValidator ctx ValidValue
validateInput tyWrappers typeDef@TypeDefinition {typeContent = tyCont, typeName} =
  withScopeType typeDef
    . validateWrapped tyWrappers tyCont
  where
    mismatchError :: [TypeWrapper] -> Maybe Message -> Value valueS -> InputValidator ctx (Value VALID)
    mismatchError wrappers = castFailure (TypeRef typeName Nothing wrappers)
    -- VALIDATION
    validateWrapped ::
      [TypeWrapper] ->
      TypeContent TRUE IN schemaS ->
      Value valueS ->
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
      (Maybe Message -> Value valueS -> InputValidator ctx ValidValue) ->
      TypeContent TRUE IN schemaS ->
      Value valueS ->
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
    Right (name, Just value) -> validatInputUnionMember (Proxy @schemaS) name value

validatInputUnionMember ::
  forall ctx schemaS valueS.
  ValueConstraints ctx schemaS valueS =>
  Proxy schemaS ->
  TypeName ->
  Value valueS ->
  InputValidator ctx (Value VALID)
validatInputUnionMember _ name value = do
  (inputDef :: TypeDefinition IN schemaS) <- askInputMember name
  validValue <- validateInputByType [TypeMaybe] inputDef value
  pure $ mkInputObject name [ObjectEntry (toFieldName name) validValue]

mkInputObject :: TypeName -> [ObjectEntry s] -> Value s
mkInputObject name xs = Object $ unsafeFromValues $ ObjectEntry "__typename" (Enum name) : xs

-- INUT Object
validateInputObject ::
  ValueConstraints ctx s valueS =>
  FieldsDefinition IN s ->
  Object valueS ->
  InputValidator ctx (Object VALID)
validateInputObject fieldsDef object =
  ordTraverse_ (`selectKnown` fieldsDef) object
    *> validateObjectWithDefaultValue fieldsDef object

validateObjectWithDefaultValue ::
  ValidateWithDefault c schemaS valueS =>
  FieldsDefinition IN schemaS ->
  Object valueS ->
  Validator (InputContext c) (Object VALID)
validateObjectWithDefaultValue fieldsDef object =
  traverseCollection (validateWithDefault object) fieldsDef

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

-- Leaf Validations
validateScalar ::
  forall m s.
  (Monad m) =>
  TypeName ->
  ScalarDefinition ->
  Value s ->
  (Maybe Message -> Value s -> m ValidValue) ->
  m ValidValue
validateScalar typeName ScalarDefinition {validateValue} value err = do
  scalarValue <- toScalar value
  case validateValue scalarValue of
    Right _ -> pure scalarValue
    Left "" -> err Nothing value
    Left message -> err (Just $ msg message) value
  where
    toScalar :: Value s -> m ValidValue
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
  (Value valueS -> m c (Value VALID)) ->
  [DataEnumValue s] ->
  Value valueS ->
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
