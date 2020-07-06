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
import Data.Maybe (Maybe (..), maybe)
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
    CONST,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    GQLErrors,
    IN,
    Message,
    OUT,
    Object,
    ObjectEntry (..),
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
    askFragments,
    askSchema,
    askVariables,
    asks,
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
    not,
    otherwise,
  )

getUnused :: (KeyOf b, KEY a ~ KEY b, Selectable ca a) => ca -> [b] -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf)

failOnUnused :: Unused ctx b => [b] -> Validator ctx ()
failOnUnused x
  | null x = pure ()
  | otherwise = do
    ctx <- Validator ask
    failure $ fmap (unused ctx) x

checkUnused :: (KeyOf b, KEY a ~ KEY b, Selectable ca a, Unused ctx b) => ca -> [b] -> Validator ctx ()
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
  forall ctx values value s.
  ( Selectable values value,
    MissingRequired values ctx,
    KEY value ~ FieldName,
    GetWith ctx Scope,
    MonadContext Validator ctx
  ) =>
  (Value s -> value) ->
  FieldDefinition IN s ->
  values ->
  Validator ctx value
selectWithDefaultValue
  f
  field@FieldDefinition
    { fieldName,
      fieldContent
    }
  values =
    selectOr
      (handeNull fieldContent)
      pure
      fieldName
      values
    where
      ------------------
      handeNull ::
        Maybe (FieldContent TRUE IN s) ->
        Validator ctx value
      handeNull (Just (DefaultInputValue value)) = pure $ f value
      handeNull Nothing
        | isNullable field = pure $ f Null
        | otherwise = failSelection
      -----------------
      failSelection = do
        ctx <- Validator ask
        position <- asks position
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
  FieldDefinition OUT VALID ->
  SelectionValidator (TypeDefinition OUT VALID)
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
  UnionMember OUT ->
  SelectionValidator (TypeName, FieldsDefinition OUT VALID)
askTypeMember UnionMember {memberName} =
  askSchema
    >>= selectOr notFound pure memberName
    >>= constraintOBJECT
  where
    notFound = do
      scopeType <- asks typename
      failure $
        "Type \""
          <> msg memberName
          <> "\" referenced by union \""
          <> msg scopeType
          <> "\" can't found in Schema."
    --------------------------------------
    constraintOBJECT ::
      TypeDefinition ANY s ->
      SelectionValidator (TypeName, FieldsDefinition OUT s)
    constraintOBJECT TypeDefinition {typeName, typeContent} = con typeContent
      where
        con DataObject {objectFields} = pure (typeName, objectFields)
        con _ = do
          scopeType <- asks typename
          failure $
            "Type \"" <> msg typeName
              <> "\" referenced by union \""
              <> msg scopeType
              <> "\" must be an OBJECT."

askInputFieldType ::
  ( Failure GQLErrors (m c),
    Failure Message (m c),
    Monad (m c),
    GetWith c (Schema s),
    MonadContext m c
  ) =>
  FieldDefinition IN s ->
  m c (TypeDefinition IN s)
askInputFieldType field@FieldDefinition {fieldName, fieldType = TypeRef {typeConName}} =
  askSchema
    >>= selectBy
      [internalError field]
      typeConName
    >>= constraintINPUT
  where
    constraintINPUT ::
      forall m s.
      ( Failure Message m,
        Monad m
      ) =>
      TypeDefinition ANY s ->
      m (TypeDefinition IN s)
    constraintINPUT x = case (fromAny x :: Maybe (TypeDefinition IN s)) of
      Just inputType -> pure inputType
      Nothing ->
        failure $
          "Type \""
            <> msg (typeName x)
            <> "\" referenced by field \""
            <> msg fieldName
            <> "\" must be an input type."

askInputMember ::
  forall c m s.
  ( GetWith c (Schema s),
    GetWith c Scope,
    Failure Message (m c),
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
    typeInfo tName =
      "Type \"" <> msg tName <> "\" referenced by inputUnion "
    notFound = do
      scopeType <- asks typename
      failure $
        typeInfo name
          <> msg scopeType
          <> "\" can't found in Schema."
    --------------------------------------
    constraintINPUT_OBJECT ::
      ( Monad (m c),
        GetWith c Scope,
        Failure Message (m c),
        MonadContext m c
      ) =>
      TypeDefinition ANY s ->
      m c (TypeDefinition IN s)
    constraintINPUT_OBJECT TypeDefinition {typeContent, ..} = con (fromAny typeContent)
      where
        con ::
          ( Monad (m c),
            GetWith c Scope,
            Failure Message (m c),
            MonadContext m c
          ) =>
          Maybe (TypeContent a IN s) ->
          m c (TypeDefinition IN s)
        con (Just content@DataInputObject {}) = pure TypeDefinition {typeContent = content, ..}
        con _ = do
          scopeType <- asks typename
          failure $
            typeInfo typeName
              <> "\""
              <> msg scopeType
              <> "\" must be an INPUT_OBJECT."

constraintInputUnion ::
  forall stage.
  [UnionMember IN] ->
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

isPosibeInputUnion :: [UnionMember IN] -> Value stage -> Either Message TypeName
isPosibeInputUnion tags (Enum name)
  | name `elem` fmap memberName tags = pure name
  | otherwise = failure $ msg name <> " is not posible union type"
isPosibeInputUnion _ _ = failure $ "\"" <> msg __inputname <> "\" must be Enum"
