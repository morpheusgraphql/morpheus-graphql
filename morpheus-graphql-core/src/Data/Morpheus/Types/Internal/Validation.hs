{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation
  ( Validator,
    SelectionValidator,
    InputValidator,
    BaseValidator,
    InputSource (..),
    OperationContext (..),
    runValidator,
    DirectiveValidator,
    askFieldType,
    askTypeMember,
    selectRequired,
    selectKnown,
    Constraint (..),
    constraint,
    withScope,
    withScopeType,
    withPosition,
    asks,
    asksScope,
    selectWithDefaultValue,
    askInputFieldType,
    askInputMember,
    startInput,
    withInputScope,
    inputMessagePrefix,
    checkUnused,
    Prop (..),
    constraintInputUnion,
    ScopeKind (..),
    withDirective,
    inputValueSource,
    askVariables,
    Scope (..),
    MissingRequired (..),
    InputContext,
    GetWith,
    SetWith,
    Unknown,
    askSchema,
    askFragments,
    MonadContext,
    CurrentSelection (..),
    askInputFieldTypeByName,
  )
where

-- MORPHEUS

import Control.Applicative (pure)
import Control.Monad (Monad ((>>=)))
import Control.Monad.Trans.Reader
  ( ask,
  )
import Data.Either (Either)
import Data.Foldable (null)
import Data.Functor ((<$>), fmap)
import Data.List (elem, filter)
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    KeyOf (..),
    Selectable,
    member,
    selectBy,
    selectOr,
    size,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    FromAny,
    GQLErrors,
    IN,
    InternalError,
    Message,
    OUT,
    Object,
    ObjectEntry (..),
    Position (..),
    Ref (..),
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    UnionMember (..),
    VALID,
    Value (..),
    __inputname,
    entryValue,
    fromAny,
    isNullable,
    msg,
    msgInternal,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.Validation.Error
  ( KindViolation (..),
    MissingRequired (..),
    Unknown (..),
    Unused (..),
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( BaseValidator,
    Constraint (..),
    CurrentSelection (..),
    DirectiveValidator,
    GetWith (..),
    InputContext,
    InputSource (..),
    InputValidator,
    MonadContext (..),
    OperationContext (..),
    Prop (..),
    Resolution,
    Scope (..),
    ScopeKind (..),
    SelectionValidator,
    SetWith (..),
    Target (..),
    Validator (..),
    ValidatorContext (..),
    askFragments,
    askSchema,
    askVariables,
    asks,
    asksScope,
    inputMessagePrefix,
    inputValueSource,
    runValidator,
    startInput,
    withDirective,
    withInputScope,
    withPosition,
    withScope,
    withScopeType,
  )
import Data.Semigroup
  ( (<>),
  )
import Prelude
  ( ($),
    (.),
    Bool (..),
    not,
    otherwise,
  )

getUnused :: (KeyOf b, KEY a ~ KEY b, Selectable a c) => c -> [b] -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf)

failOnUnused :: Unused ctx b => [b] -> Validator ctx ()
failOnUnused x
  | null x = pure ()
  | otherwise = do
    ctx <- unValidatorContext <$> Validator ask
    failure $ fmap (unused ctx) x

checkUnused :: (KeyOf b, KEY a ~ KEY b, Selectable a ca, Unused ctx b) => ca -> [b] -> Validator ctx ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  forall (a :: Target) inp ctx s.
  KindViolation a inp =>
  Constraint (a :: Target) ->
  inp ->
  TypeDefinition ANY s ->
  Validator ctx (Resolution s a)
constraint OBJECT _ TypeDefinition {typeContent = DataObject {objectFields}, typeName} =
  pure (typeName, objectFields)
constraint INPUT ctx x = maybe (failure [kindViolation INPUT ctx]) pure (fromAny x)
constraint target ctx _ = failure [kindViolation target ctx]

selectRequired ::
  ( Selectable value c,
    MissingRequired c ctx,
    KEY Ref ~ KEY value
  ) =>
  Ref ->
  c ->
  Validator ctx value
selectRequired selector container =
  do
    ValidatorContext scope ctx <- Validator ask
    selectBy
      [missingRequired scope ctx selector container]
      (keyOf selector)
      container

selectWithDefaultValue ::
  forall ctx values value s validValue.
  ( Selectable value values,
    MissingRequired values ctx,
    KEY value ~ FieldName,
    MonadContext Validator ctx
  ) =>
  (Value s -> Validator ctx validValue) ->
  (value -> Validator ctx validValue) ->
  FieldDefinition IN s ->
  values ->
  Validator ctx validValue
selectWithDefaultValue
  f
  validateF
  field@FieldDefinition
    { fieldName,
      fieldContent
    }
  values =
    selectOr
      (handeNull fieldContent)
      validateF
      fieldName
      values
    where
      ------------------
      handeNull ::
        Maybe (FieldContent TRUE IN s) ->
        Validator ctx validValue
      handeNull (Just (DefaultInputValue value)) = f value
      handeNull Nothing
        | isNullable field = f Null
        | otherwise = failSelection
      -----------------
      failSelection = do
        ValidatorContext scope ctx <- Validator ask
        position <- asksScope position
        failure [missingRequired scope ctx (Ref fieldName (fromMaybe (Position 0 0) position)) values]

selectKnown ::
  ( Selectable a c,
    Unknown c sel ctx,
    KeyOf sel,
    KEY sel ~ KEY a
  ) =>
  sel ->
  c ->
  Validator ctx a
selectKnown selector lib =
  do
    ValidatorContext scope ctx <- Validator ask
    selectBy
      (unknown scope ctx lib selector)
      (keyOf selector)
      lib

askFieldType ::
  FieldDefinition OUT VALID ->
  SelectionValidator (TypeDefinition OUT VALID)
askFieldType field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  askSchema
    >>= selectBy (unknownType field) typeConName
    >>= internalConstraint (fieldTypeViolation False field)

askTypeMember ::
  UnionMember OUT s ->
  SelectionValidator (TypeName, FieldsDefinition OUT VALID)
askTypeMember UnionMember {memberName} =
  askSchema
    >>= selectOr notFound pure memberName
    >>= constraintOBJECT
  where
    notFound = do
      scopeType <- asksScope typename
      failure
        (unknownUnionType False scopeType memberName)
    --------------------------------------
    constraintOBJECT ::
      TypeDefinition ANY s ->
      SelectionValidator (TypeName, FieldsDefinition OUT s)
    constraintOBJECT t@TypeDefinition {typeName, typeContent} = con typeContent
      where
        con DataObject {objectFields} = pure (typeName, objectFields)
        con _ = do
          scopeType <- asksScope typename
          failure (unionTypeViolation False scopeType t)

askInputFieldTypeByName ::
  ( Failure GQLErrors (m c),
    Failure InternalError (m c),
    Monad (m c),
    GetWith c (Schema s),
    MonadContext m c
  ) =>
  TypeName ->
  m c (TypeDefinition IN s)
askInputFieldTypeByName name =
  askSchema
    >>= selectBy (unknownInputType name) name
    >>= internalConstraint inputTypeViolation

askInputFieldType ::
  ( Failure GQLErrors (m c),
    Failure InternalError (m c),
    Monad (m c),
    GetWith c (Schema s),
    MonadContext m c
  ) =>
  FieldDefinition IN s ->
  m c (TypeDefinition IN s)
askInputFieldType field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  askSchema
    >>= selectBy (unknownType field) typeConName
    >>= internalConstraint (fieldTypeViolation True field)

askInputMember ::
  forall c m s.
  ( GetWith c (Schema s),
    Failure InternalError (m c),
    Monad (m c),
    MonadContext m c
  ) =>
  TypeName ->
  m c (TypeDefinition IN s)
askInputMember name =
  askSchema
    >>= selectOr notFound pure name
    >>= constraintINPUT_OBJECT
  where
    notFound = do
      scopeType <- asksScope typename
      failure (unknownUnionType True scopeType name)
    --------------------------------------
    constraintINPUT_OBJECT ::
      ( Monad (m c),
        Failure InternalError (m c),
        MonadContext m c
      ) =>
      TypeDefinition ANY s ->
      m c (TypeDefinition IN s)
    constraintINPUT_OBJECT t@TypeDefinition {typeContent, ..} = con (fromAny typeContent)
      where
        con ::
          ( Monad (m c),
            Failure InternalError (m c),
            MonadContext m c
          ) =>
          Maybe (TypeContent a IN s) ->
          m c (TypeDefinition IN s)
        con (Just content@DataInputObject {}) = pure TypeDefinition {typeContent = content, ..}
        con _ = do
          scopeType <- asksScope typename
          failure (unionTypeViolation True scopeType t)

constraintInputUnion ::
  forall stage schemaStage.
  [UnionMember IN schemaStage] ->
  Object stage ->
  Either Message (TypeName, Maybe (Value stage))
constraintInputUnion tags hm = do
  (enum :: Value stage) <-
    entryValue
      <$> selectBy
        ( "valid input union should contain \""
            <> msg __inputname
            <> "\" and actual value"
        )
        __inputname
        hm
  tyName <- isPosibeInputUnion tags enum
  case size hm of
    1 -> pure (tyName, Nothing)
    2 -> do
      value <-
        entryValue
          <$> selectBy
            ( "value for Union \""
                <> msg tyName
                <> "\" was not Provided."
            )
            (toFieldName tyName)
            hm
      pure (tyName, Just value)
    _ -> failure ("input union can have only one variant." :: Message)

isPosibeInputUnion :: [UnionMember IN s] -> Value stage -> Either Message TypeName
isPosibeInputUnion tags (Enum name)
  | name `elem` fmap memberName tags = pure name
  | otherwise = failure $ msg name <> " is not posible union type"
isPosibeInputUnion _ _ = failure $ "\"" <> msg __inputname <> "\" must be Enum"

unknownInputType :: TypeName -> InternalError
unknownInputType name =
  "Type " <> msgInternal name <> " can't found in Schema "

unknownType :: FieldDefinition cat s -> InternalError
unknownType
  FieldDefinition
    { fieldName,
      fieldType = TypeRef {typeConName}
    } =
    "Type " <> msgInternal typeConName
      <> " referenced by field "
      <> msgInternal fieldName
      <> " can't found in Schema "

unknownUnionType ::
  Bool ->
  TypeName ->
  TypeName ->
  InternalError
unknownUnionType
  isInput
  scopeType
  memberName =
    "Type \""
      <> msgInternal memberName
      <> "\" referenced by "
      <> refType
      <> " union \""
      <> msgInternal scopeType
      <> "\" can't found in Schema."
    where
      refType
        | isInput = "INPUT_OBJECT"
        | otherwise = "OBJECT"

inputTypeViolation :: TypeDefinition c s -> InternalError
inputTypeViolation x =
  "Type "
    <> msgInternal (typeName x)
    <> " must be an Input Type."

fieldTypeViolation ::
  Bool ->
  FieldDefinition cat s ->
  TypeDefinition cat' s' ->
  InternalError
fieldTypeViolation isInput field anyType =
  "Type \"" <> msgInternal (typeName anyType)
    <> "\" referenced by "
    <> refType
    <> " \""
    <> msgInternal (fieldName field)
    <> "\" must be an"
    <> mustBe
    <> "."
  where
    refType
      | isInput = "INPUT_OBJECT"
      | otherwise = "OBJECT"
    mustBe
      | isInput = "INPUT_TYPE"
      | otherwise = "OUTPUT_TYPE"

unionRefType :: Bool -> InternalError
unionRefType isInput
  | isInput = "INPUT_UNION"
  | otherwise = "UNION"

unionTypeViolation ::
  Bool ->
  TypeName ->
  TypeDefinition cat' s' ->
  InternalError
unionTypeViolation isInput scopeType x =
  "Type \"" <> msgInternal (typeName x)
    <> "\" referenced by "
    <> unionRefType isInput
    <> " \""
    <> msgInternal scopeType
    <> "\" must be an "
    <> mustBe
    <> "."
  where
    mustBe
      | isInput = "INPUT_OBJECT"
      | otherwise = "OUTPUT_OBJECT"

internalConstraint ::
  ( FromAny a k,
    Failure InternalError f
  ) =>
  (a ANY s -> InternalError) ->
  a ANY s ->
  f (a k s)
internalConstraint err anyType = case fromAny anyType of
  Just x -> pure x
  Nothing -> failure (err anyType)
