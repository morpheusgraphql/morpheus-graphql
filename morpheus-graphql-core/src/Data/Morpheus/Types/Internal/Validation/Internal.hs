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
    askInputMember,
    getOperationObjectType,
    askInputFieldType,
  )
where

-- MORPHEUS

import Control.Applicative (pure)
import Control.Monad (Monad ((>>=)))
import Data.Maybe (maybe)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    selectBy,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldsDefinition,
    FromAny,
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
    Bool (..),
    otherwise,
  )

askFieldType ::
  TypeRef ->
  SelectionValidator (TypeDefinition OUT VALID)
askFieldType TypeRef {typeConName} =
  askSchema
    >>= selectBy (unknownType typeConName) typeConName
    >>= kindConstraint (categoryViolation False)

askInputFieldType ::
  ( Failure InternalError (m c),
    Monad (m c),
    GetWith c (Schema s),
    MonadContext m c
  ) =>
  TypeRef ->
  m c (TypeDefinition IN s)
askInputFieldType TypeRef {typeConName} =
  askSchema
    >>= selectBy (unknownType typeConName) typeConName
    >>= kindConstraint (categoryViolation True)

askTypeMember ::
  UnionMember OUT s ->
  SelectionValidator
    ( TypeDefinition OUT VALID,
      FieldsDefinition OUT VALID
    )
askTypeMember UnionMember {memberName} =
  askSchema
    >>= selectOr notFound pure memberName
    >>= kindConstraint (unionTypeViolation False)
    >>= constraintOBJECT
  where
    notFound = failure (unknownType memberName)
    --------------------------------------
    constraintOBJECT ::
      TypeDefinition OUT s ->
      SelectionValidator (TypeDefinition OUT s, FieldsDefinition OUT s)
    constraintOBJECT t@TypeDefinition {typeContent, typeName} = con typeContent
      where
        con DataObject {objectFields} = pure (t, objectFields)
        con _ = failure (unionTypeViolation False typeName)

askInputMember ::
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
    >>= kindConstraint (categoryViolation True)
    >>= constraintInputObject
  where
    notFound = failure (unknownType name)

constraintInputObject ::
  forall c m s.
  ( Monad (m c),
    Failure InternalError (m c),
    MonadContext m c
  ) =>
  TypeDefinition IN s ->
  m c (TypeDefinition IN s)
constraintInputObject typeDef@TypeDefinition {typeContent = DataInputObject {}} = pure typeDef
constraintInputObject TypeDefinition {typeName} = failure (unionTypeViolation True typeName)

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

categoryViolation ::
  Bool ->
  TypeName ->
  InternalError
categoryViolation isInput typeName =
  "Type \"" <> msgInternal typeName
    <> "\" must be an"
    <> mustBeCategory isInput
    <> "."

mustBeCategory :: Bool -> InternalError
mustBeCategory True = "input type"
mustBeCategory False = " output type"

mustBeKind :: InternalError -> Bool -> InternalError
mustBeKind kind isInput
  | isInput = "INPUT_" <> kind
  | otherwise = "OUTPUT" <> kind

unionTypeViolation ::
  Bool ->
  TypeName ->
  InternalError
unionTypeViolation isInput typeName =
  "Type \"" <> msgInternal typeName <> "\" must be an " <> mustBe <> "."
  where
    mustBe = mustBeKind "OBJECT" isInput

kindConstraint ::
  ( Failure InternalError f,
    FromAny TypeDefinition k
  ) =>
  (TypeName -> InternalError) ->
  TypeDefinition ANY s ->
  f (TypeDefinition k s)
kindConstraint err anyType =
  maybe
    (failure $ err (typeName anyType))
    pure
    (fromAny anyType)
