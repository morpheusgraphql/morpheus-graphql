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

module Data.Morpheus.Types.Internal.Validation
  ( Validator,
    SelectionValidator,
    InputValidator,
    BaseValidator,
    InputSource (..),
    OperationContext (..),
    runValidator,
    askFieldType,
    askTypeMember,
    selectRequired,
    selectKnown,
    Constraint (..),
    constraint,
    withScope,
    withScopeType,
    withPosition,
    askScopeTypeName,
    selectWithDefaultValue,
    askPosition,
    askInputFieldType,
    askInputMember,
    askSchema,
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
    askFragments,
    Scope (..),
    MissingRequired (..),
    InputContext,
    WithScope,
    WithSchema,
    Unknown,
    WithInput,
  )
where

import Control.Monad.Trans.Reader
  ( ask,
  )
-- MORPHEUS

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
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    GQLErrors,
    IN,
    Message,
    OUT,
    Object,
    Ref (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    Value (..),
    __inputname,
    entryValue,
    fromAny,
    isFieldNullable,
    msg,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.Validation.Error
  ( InternalError (..),
    KindViolation (..),
    MissingRequired (..),
    Unknown (..),
    Unused (..),
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( BaseValidator,
    Constraint (..),
    InputContext,
    InputSource (..),
    InputValidator,
    OperationContext (..),
    Prop (..),
    Resolution,
    Scope (..),
    ScopeKind (..),
    SelectionValidator,
    Target (..),
    Validator (..),
    WithInput,
    WithSchema (..),
    WithScope (..),
    WithSelection (..),
    WithVariables (..),
    askPosition,
    askScopeTypeName,
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

getUnused :: (KeyOf b, KEY a ~ KEY b, Selectable ca a) => ca -> [b] -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf)

failOnUnused :: Unused ctx b => [b] -> Validator ctx ()
failOnUnused x
  | null x = return ()
  | otherwise = do
    ctx <- Validator ask
    failure $ map (unused ctx) x

checkUnused :: (KeyOf b, KEY a ~ KEY b, Selectable ca a, Unused ctx b) => ca -> [b] -> Validator ctx ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  forall (a :: Target) inp ctx.
  KindViolation a inp =>
  Constraint (a :: Target) ->
  inp ->
  TypeDefinition ANY ->
  Validator ctx (Resolution a)
constraint OBJECT _ TypeDefinition {typeContent = DataObject {objectFields}, typeName} =
  pure (typeName, objectFields)
constraint INPUT ctx x = maybe (failure [kindViolation INPUT ctx]) pure (fromAny x)
constraint target ctx _ = failure [kindViolation target ctx]

selectRequired ::
  ( Selectable c value,
    MissingRequired c ctx,
    KEY Ref ~ KEY value
  ) =>
  Ref ->
  c ->
  Validator ctx value
selectRequired selector container =
  do
    ctx <- Validator ask
    selectBy
      [missingRequired ctx selector container]
      (keyOf selector)
      container

selectWithDefaultValue ::
  ( Selectable values value,
    MissingRequired values ctx,
    KEY value ~ FieldName,
    WithScope (Validator ctx)
  ) =>
  value ->
  FieldDefinition IN ->
  values ->
  Validator ctx value
selectWithDefaultValue
  fallbackValue
  field@FieldDefinition {fieldName}
  values =
    selectOr
      handleNullable
      pure
      fieldName
      values
    where
      ------------------
      handleNullable
        | isFieldNullable field = pure fallbackValue
        | otherwise = failSelection
      -----------------
      failSelection = do
        ctx <- Validator ask
        position <- askPosition
        failure [missingRequired ctx (Ref fieldName position) values]

selectKnown ::
  ( Selectable c a,
    Unknown c ctx,
    KeyOf sel,
    sel ~ UnknownSelector c,
    KEY sel ~ KEY a
  ) =>
  sel ->
  c ->
  Validator ctx a
selectKnown selector lib =
  do
    ctx <- Validator ask
    selectBy
      (unknown ctx lib selector)
      (keyOf selector)
      lib

askFieldType ::
  FieldDefinition OUT ->
  SelectionValidator (TypeDefinition OUT)
askFieldType field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  do
    schema <- askSchema
    anyType <-
      selectBy
        [internalError field]
        typeConName
        schema
    case fromAny anyType of
      Just x -> pure x
      Nothing ->
        failure $
          "Type \"" <> msg (typeName anyType)
            <> "\" referenced by OBJECT \""
            <> "\" must be an OUTPUT_TYPE."

askTypeMember ::
  TypeName ->
  SelectionValidator (TypeName, FieldsDefinition OUT)
askTypeMember name =
  askSchema
    >>= selectOr notFound pure name
    >>= constraintOBJECT
  where
    notFound = do
      scopeType <- askScopeTypeName
      failure $
        "Type \""
          <> msg name
          <> "\" referenced by union \""
          <> msg scopeType
          <> "\" can't found in Schema."
    --------------------------------------
    constraintOBJECT :: TypeDefinition ANY -> SelectionValidator (TypeName, FieldsDefinition OUT)
    constraintOBJECT TypeDefinition {typeName, typeContent} = con typeContent
      where
        con DataObject {objectFields} = pure (typeName, objectFields)
        con _ = do
          scopeType <- askScopeTypeName
          failure $
            "Type \"" <> msg typeName
              <> "\" referenced by union \""
              <> msg scopeType
              <> "\" must be an OBJECT."

askInputFieldType ::
  ( Failure GQLErrors m,
    Failure Message m,
    Monad m,
    WithSchema m
  ) =>
  FieldDefinition IN ->
  m (TypeDefinition IN)
askInputFieldType field@FieldDefinition {fieldName, fieldType = TypeRef {typeConName}} =
  askSchema
    >>= selectBy
      [internalError field]
      typeConName
    >>= constraintINPUT
  where
    constraintINPUT ::
      ( Failure Message m,
        Monad m
      ) =>
      TypeDefinition ANY ->
      m (TypeDefinition IN)
    constraintINPUT x = case (fromAny x :: Maybe (TypeDefinition IN)) of
      Just inputType -> pure inputType
      Nothing ->
        failure $
          "Type \""
            <> msg (typeName x)
            <> "\" referenced by field \""
            <> msg fieldName
            <> "\" must be an input type."

askInputMember ::
  ( WithSchema m,
    WithScope m,
    Failure Message m,
    Monad m
  ) =>
  TypeName ->
  m (TypeDefinition IN)
askInputMember name =
  askSchema
    >>= selectOr notFound pure name
    >>= constraintINPUT_OBJECT
  where
    typeInfo tName =
      "Type \"" <> msg tName <> "\" referenced by inputUnion "
    notFound = do
      scopeType <- askScopeTypeName
      failure $
        typeInfo name
          <> msg scopeType
          <> "\" can't found in Schema."
    --------------------------------------
    constraintINPUT_OBJECT ::
      ( Monad m,
        WithScope m,
        Failure Message m
      ) =>
      TypeDefinition ANY ->
      m (TypeDefinition IN)
    constraintINPUT_OBJECT TypeDefinition {typeContent, ..} = con (fromAny typeContent)
      where
        con ::
          ( Monad m,
            WithScope m,
            Failure Message m
          ) =>
          Maybe (TypeContent a IN) ->
          m (TypeDefinition IN)
        con (Just content@DataInputObject {}) = pure TypeDefinition {typeContent = content, ..}
        con _ = do
          scopeType <- askScopeTypeName
          failure $
            typeInfo typeName
              <> "\""
              <> msg scopeType
              <> "\" must be an INPUT_OBJECT."

constraintInputUnion ::
  forall stage.
  [(TypeName, Bool)] ->
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

isPosibeInputUnion :: [(TypeName, Bool)] -> Value stage -> Either Message TypeName
isPosibeInputUnion tags (Enum name) = case lookup name tags of
  Nothing ->
    failure
      ( msg name
          <> " is not posible union type"
      )
  _ -> pure name
isPosibeInputUnion _ _ =
  failure $
    "\""
      <> msg __inputname
      <> "\" must be Enum"
