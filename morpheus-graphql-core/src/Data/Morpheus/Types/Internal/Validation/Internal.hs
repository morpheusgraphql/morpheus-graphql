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

module Data.Morpheus.Types.Internal.Validation.Internal
  ( askFieldType,
    askTypeMember,
    askInputFieldTypeByName,
    askInputMember,
    getOperationObjectType,
    askInputFieldType,
  )
where

-- MORPHEUS

import Control.Applicative (pure)
import Control.Monad (Monad ((>>=)))
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    selectBy,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition (..),
    FieldsDefinition,
    FromAny,
    GQLErrors,
    IN,
    InternalError,
    OUT,
    Operation,
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    UnionMember (..),
    VALID,
    fromAny,
    getOperationDataType,
    msgInternal,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( GetWith,
    MonadContext,
    SelectionValidator,
    askSchema,
  )
import Data.Semigroup
  ( (<>),
  )
import Prelude
  ( ($),
    (.),
    Bool (..),
    otherwise,
  )

askFieldType ::
  FieldDefinition OUT VALID ->
  SelectionValidator (TypeDefinition OUT VALID)
askFieldType field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  askSchema
    >>= selectBy (unknownType typeConName) typeConName
    >>= internalConstraint (fieldTypeViolation False field)

askTypeMember ::
  UnionMember OUT s ->
  SelectionValidator (TypeDefinition OUT VALID, FieldsDefinition OUT VALID)
askTypeMember UnionMember {memberName} =
  askSchema
    >>= selectOr notFound pure memberName
    >>= internalConstraint (unionTypeViolation False)
    >>= constraintOBJECT
  where
    notFound = failure (unknownType memberName)
    --------------------------------------
    constraintOBJECT ::
      TypeDefinition OUT s ->
      SelectionValidator (TypeDefinition OUT s, FieldsDefinition OUT s)
    constraintOBJECT t@TypeDefinition {typeContent} = con typeContent
      where
        con DataObject {objectFields} = pure (t, objectFields)
        con _ = failure (unionTypeViolation False t)

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
    >>= selectBy (unknownType name) name
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
    >>= selectBy (unknownType typeConName) typeConName
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
    notFound = failure (unknownType name)
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
        con _ = failure (unionTypeViolation True t)

getOperationObjectType :: Operation a -> SelectionValidator (TypeDefinition OUT VALID, FieldsDefinition OUT VALID)
getOperationObjectType operation = do
  dt <- askSchema >>= getOperationDataType operation
  case dt of
    TypeDefinition {typeContent = DataObject {objectFields, ..}, typeName, ..} ->
      pure
        ( TypeDefinition {typeContent = DataObject {objectFields, ..}, ..},
          objectFields
        )
    TypeDefinition {typeName} ->
      failure
        ( "Type Mismatch: operation \""
            <> msgInternal typeName
            <> "\" must be an Object" ::
            InternalError
        )

unknownType :: TypeName -> InternalError
unknownType name = "Type \"" <> msgInternal name <> "\" can't found in Schema."

inputTypeViolation :: TypeDefinition c s -> InternalError
inputTypeViolation x = "Type " <> msgInternal (typeName x) <> " must be an Input Type."

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
    refType = mustBeKind "OBJECT" isInput
    mustBe = mustBeKind "TYPE" isInput

mustBeKind :: InternalError -> Bool -> InternalError
mustBeKind kind isInput
  | isInput = "INPUT_" <> kind
  | otherwise = "OUTPUT" <> kind

unionTypeViolation ::
  Bool ->
  TypeDefinition cat' s' ->
  InternalError
unionTypeViolation isInput x =
  "Type \"" <> msgInternal (typeName x) <> "\" must be an " <> mustBe <> "."
  where
    mustBe = mustBeKind "OBJECT" isInput

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
