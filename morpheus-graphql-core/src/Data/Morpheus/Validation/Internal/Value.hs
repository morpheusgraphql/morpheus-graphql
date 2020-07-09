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
  ( Validate (..),
    ValueContext (..),
  )
where

import Control.Applicative ((*>), pure)
import Control.Monad (Monad ((>>=)))
import Data.Either (Either (..))
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<$>), fmap)
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
    SetWith,
    Validate (..),
    Validator,
    askInputFieldType,
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

type InputConstraints ctx schemaS s =
  ( GetWith ctx (Schema schemaS),
    GetWith (InputContext ctx) InputSource,
    SetWith ctx Scope,
    Validate (ValueContext schemaS) ObjectEntry s (InputContext ctx)
  )

instance
  ( InputConstraints ctx VALID CONST,
    ValidateWith ctx VALID CONST
  ) =>
  Validate (ValueContext VALID) ObjectEntry CONST (InputContext ctx)
  where
  validate (ValueContext x y) obj@(ObjectEntry name _) =
    ObjectEntry name <$> validateInput x y obj

instance
  ( InputConstraints ctx CONST CONST,
    GetWith ctx (Schema CONST),
    (ValidateWith ctx CONST CONST)
  ) =>
  Validate (ValueContext CONST) ObjectEntry CONST (InputContext ctx)
  where
  validate (ValueContext x y) obj@(ObjectEntry name _) =
    ObjectEntry name <$> validateInput x y obj

data ValueContext s = ValueContext
  { valueWrappers :: [TypeWrapper],
    valueTypeDef :: TypeDefinition IN s
  }

-- Validate input Values
validateInput ::
  forall ctx s.
  ( InputConstraints ctx s CONST,
    ValidateWith ctx s CONST,
    ValidateWith ctx s s
  ) =>
  [TypeWrapper] ->
  TypeDefinition IN s ->
  ObjectEntry CONST ->
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
  ( GetWith ctx (Schema schemaS),
    Validate (ValueContext schemaS) ObjectEntry s (InputContext ctx)
  ) =>
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
  forall ctx schemaS s.
  ( Validate (ValueContext schemaS) ObjectEntry s (InputContext ctx),
    GetWith ctx (Schema schemaS)
  ) =>
  (TypeDefinition IN schemaS -> ValueContext schemaS) ->
  TypeName ->
  Value s ->
  InputValidator ctx (Value VALID)
validatInputUnionMember f name value = do
  (inputDef :: TypeDefinition IN schemaS) <- askInputMember name
  validValue <-
    validate
      (f inputDef)
      (ObjectEntry (toFieldName name) value)
  pure $ mkInputObject name [validValue]

mkInputObject :: TypeName -> [ObjectEntry s] -> Value s
mkInputObject name xs = Object $ unsafeFromValues $ ObjectEntry "__typename" (Enum name) : xs

-- INUT Object
validateInputObject ::
  ( InputConstraints ctx s CONST,
    ValidateWith ctx s CONST,
    ValidateWith ctx s s
  ) =>
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
  ( InputConstraints ctx s CONST,
    ValidateWith ctx s CONST
  ) =>
  FieldsDefinition IN s ->
  ObjectEntry CONST ->
  InputValidator ctx (ObjectEntry VALID)
validateField parentFields entry = do
  field <- selectKnown entry parentFields
  validateInputField field entry

validateObjectWithDefaultValue ::
  ( InputConstraints c s CONST,
    ValidateWith c s s,
    ValidateWith c s CONST
  ) =>
  FieldsDefinition IN s ->
  Object CONST ->
  Validator (InputContext c) (Object VALID)
validateObjectWithDefaultValue fieldsDef object =
  traverse (validateFieldWithDefaultValue object) (elems fieldsDef)
    >>= fromElems

validateFieldWithDefaultValue ::
  forall s c s'.
  ( InputConstraints c s s',
    ValidateWith c s s,
    ValidateWith c s s'
  ) =>
  Object s' ->
  FieldDefinition IN s ->
  Validator (InputContext c) (ObjectEntry VALID)
validateFieldWithDefaultValue object fieldDef@FieldDefinition {fieldName} =
  selectWithDefaultValue __validate (validateInputField fieldDef) fieldDef object
  where
    __validate ::
      Value s ->
      Validator (InputContext c) (ObjectEntry VALID)
    __validate = validateInputField fieldDef . ObjectEntry fieldName

class ValidateWith c schemaS s where
  validateInputField ::
    FieldDefinition IN schemaS ->
    ObjectEntry s ->
    Validator (InputContext c) (ObjectEntry VALID)

instance ValidateWith c VALID VALID where
  validateInputField _ = pure

instance
  ( GetWith c (Schema VALID)
  ) =>
  ValidateWith c VALID CONST
  where
  validateInputField fieldDef@FieldDefinition {fieldName, fieldType = TypeRef {typeConName, typeWrappers}} entry = do
    inputTypeDef <- askInputFieldType fieldDef
    withInputScope (Prop fieldName typeConName) $
      validate
        ( ValueContext
            typeWrappers
            inputTypeDef
        )
        entry

--(InputConstraints c schemaS s) =>
instance
  ( GetWith c (Schema CONST)
  ) =>
  ValidateWith c CONST CONST
  where
  validateInputField fieldDef@FieldDefinition {fieldName, fieldType = TypeRef {typeConName, typeWrappers}} entry = do
    inputTypeDef <- askInputFieldType fieldDef
    withInputScope (Prop fieldName typeConName) $
      validate
        ( ValueContext
            typeWrappers
            inputTypeDef
        )
        entry

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
