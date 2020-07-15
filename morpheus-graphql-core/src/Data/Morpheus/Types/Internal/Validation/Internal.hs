{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation.Internal
  ( askType,
    askTypeMember,
    getOperationType,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative, pure)
import Control.Monad ((>=>), Monad ((>>=)))
import Data.Maybe (maybe)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldsDefinition,
    FromAny,
    IN,
    InternalError,
    OUT,
    Operation,
    Operation (..),
    Schema (..),
    TRUE,
    Token,
    TypeContent (..),
    TypeContent,
    TypeDefinition (..),
    TypeName (..),
    TypeRef,
    Typed,
    UnionMember (..),
    VALID,
    fromAny,
    getOperationDataType,
    msgInternal,
    typeConName,
    untyped,
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
  )

askType ::
  Constraints m c cat s =>
  Typed cat s TypeRef ->
  m c (TypeDefinition cat s)
askType = untyped (__askType . typeConName)

__askType ::
  Constraints m c cat s => TypeName -> m c (TypeDefinition cat s)
__askType name =
  askSchema
    >>= selectBy (unknownType name) name
    >>= kindConstraint

askTypeMember ::
  Constraints m c cat s =>
  UnionMember cat s ->
  m c (TypeDefinition cat s, FieldsDefinition cat s)
askTypeMember = __askType . memberName >=> constraintObject

type Constraints m c cat s =
  ( Failure InternalError (m c),
    Monad (m c),
    MonadContext m c,
    GetWith c (Schema s),
    KindErrors cat,
    FromAny (TypeContent TRUE) cat
  )

getOperationType :: Operation a -> SelectionValidator (TypeDefinition OUT VALID, FieldsDefinition OUT VALID)
getOperationType operation =
  askSchema
    >>= getOperationDataType operation
    >>= constraintObject

unknownType :: TypeName -> InternalError
unknownType name = "Type \"" <> msgInternal name <> "\" can't found in Schema."

_kindConstraint ::
  ( Failure InternalError f,
    FromAny TypeDefinition k
  ) =>
  Token ->
  TypeDefinition ANY s ->
  f (TypeDefinition k s)
_kindConstraint err anyType =
  maybe
    (failure $ violation err (typeName anyType))
    pure
    (fromAny anyType)

class KindErrors c where
  kindConstraint ::
    ( Failure InternalError f,
      FromAny TypeDefinition c
    ) =>
    TypeDefinition ANY s ->
    f (TypeDefinition c s)
  constraintObject ::
    ( Applicative f,
      Failure InternalError f
    ) =>
    TypeDefinition c s ->
    f (TypeDefinition c s, FieldsDefinition c s)

instance KindErrors IN where
  kindConstraint = _kindConstraint "input type"
  constraintObject typeDef@TypeDefinition {typeContent = DataInputObject inputFields} = pure (typeDef, inputFields)
  constraintObject TypeDefinition {typeName} = failure (violation "input object" typeName)

instance KindErrors OUT where
  kindConstraint = _kindConstraint "output type"
  constraintObject typeDef@TypeDefinition {typeContent = DataObject {objectFields}} = pure (typeDef, objectFields)
  constraintObject TypeDefinition {typeName} = failure (violation "object" typeName)

violation ::
  Token ->
  TypeName ->
  InternalError
violation kind typeName =
  "Type \"" <> msgInternal typeName
    <> "\" must be an"
    <> msgInternal kind
    <> "."
