{-# LANGUAGE ConstraintKinds #-}
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
    OUTPUT_OBJECT,
    Operation,
    Operation (..),
    Stage,
    TRUE,
    Token,
    TypeCategory,
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
    typed,
    untyped,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( MonadContext,
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

askType2 ::
  Constraints m c cat s =>
  Typed cat s TypeName ->
  m c (TypeDefinition cat s)
askType2 = untyped __askType

__askType ::
  Constraints m c cat s => TypeName -> m c (TypeDefinition cat s)
__askType name =
  askSchema
    >>= selectBy (unknownType name) name
    >>= kindConstraint

askTypeMember ::
  Constraints m c cat s =>
  UnionMember cat s ->
  m c (TypeMemberResponse cat s)
askTypeMember = askType2 . typed memberName >=> constraintObject

type family TypeMemberResponse (cat :: TypeCategory) (s :: Stage)

type instance TypeMemberResponse OUT s = TypeDefinition OUTPUT_OBJECT s

type instance TypeMemberResponse IN s = (TypeDefinition IN s, FieldsDefinition IN s)

type Constraints m c cat s =
  ( Failure InternalError (m c),
    Monad (m c),
    MonadContext m s c,
    KindErrors cat,
    FromAny (TypeContent TRUE) cat
  )

getOperationType :: Operation a -> SelectionValidator (TypeDefinition OUTPUT_OBJECT VALID)
getOperationType operation = askSchema >>= getOperationDataType operation

unknownType :: TypeName -> InternalError
unknownType name = "Type \"" <> msgInternal name <> "\" can't found in Schema."

type KindConstraint f c =
  ( Failure InternalError f,
    FromAny TypeDefinition c
  )

_kindConstraint ::
  KindConstraint f k =>
  Token ->
  TypeDefinition ANY s ->
  f (TypeDefinition k s)
_kindConstraint err anyType =
  maybe
    (failure $ violation err (typeName anyType))
    pure
    (fromAny anyType)

class KindErrors c where
  kindConstraint :: KindConstraint f c => TypeDefinition ANY s -> f (TypeDefinition c s)
  constraintObject ::
    ( Applicative f,
      Failure InternalError f
    ) =>
    TypeDefinition c s ->
    f (TypeMemberResponse c s)

instance KindErrors IN where
  kindConstraint = _kindConstraint "input type"
  constraintObject typeDef@TypeDefinition {typeContent = DataInputObject inputFields} = pure (typeDef, inputFields)
  constraintObject TypeDefinition {typeName} = failure (violation "input object" typeName)

instance KindErrors OUT where
  kindConstraint = _kindConstraint "output type"
  constraintObject TypeDefinition {typeContent = DataObject {..}, ..} = pure TypeDefinition {typeContent = DataObject {..}, ..}
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
