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
    askInterfaceTypes,
  )
where

import Data.Morpheus.Internal.Utils
  ( Failure (..),
    fromElems,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FromCategory,
    IMPLEMENTABLE,
    IN,
    InternalError,
    OBJECT,
    OUT,
    Operation,
    Operation (..),
    OrdMap,
    TRUE,
    ToOBJECT,
    Token,
    TypeName,
    TypeRef,
    UnionMember (..),
    VALID,
    ValidationErrors,
    fromAny,
    fromCategory,
    getOperationDataType,
    msgInternal,
    typeConName,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
import Data.Morpheus.Types.Internal.Validation.Validator
  ( MonadContext,
    SelectionValidator,
    askSchema,
  )
import Relude

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
    >>= maybe (failure (unknownType name)) pure . lookupDataType name
    >>= kindConstraint

askTypeMember ::
  Constraints m c cat s =>
  UnionMember cat s ->
  m c (TypeDefinition (ToOBJECT cat) s)
askTypeMember = askType2 . typed memberName >=> constraintObject

askInterfaceTypes ::
  ( Failure InternalError (m c),
    Failure ValidationErrors (m c),
    Monad (m c),
    MonadContext m s c,
    FromCategory (TypeContent TRUE) ANY IMPLEMENTABLE
  ) =>
  TypeDefinition IMPLEMENTABLE s ->
  m c (OrdMap TypeName (TypeDefinition IMPLEMENTABLE s))
askInterfaceTypes typeDef@TypeDefinition {typeName} =
  askSchema
    >>= traverse (validate . fromCategory) . possibleInterfaceTypes typeName
    >>= fromElems . (typeDef :)
  where
    validate (Just x) = pure x
    validate Nothing = failure ("TODO: invalid interface Types" :: InternalError)

type Constraints m c cat s =
  ( Failure InternalError (m c),
    Monad (m c),
    MonadContext m s c,
    KindErrors cat,
    FromCategory (TypeContent TRUE) ANY cat
  )

getOperationType :: Operation a -> SelectionValidator (TypeDefinition OBJECT VALID)
getOperationType operation = askSchema >>= getOperationDataType operation

unknownType :: TypeName -> InternalError
unknownType name = "Type \"" <> msgInternal name <> "\" can't found in Schema."

type KindConstraint f c =
  ( Failure InternalError f,
    FromCategory TypeDefinition ANY c
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
    f (TypeDefinition (ToOBJECT c) s)

instance KindErrors IN where
  kindConstraint = _kindConstraint "input type"
  constraintObject TypeDefinition {typeContent = DataInputObject {..}, ..} = pure TypeDefinition {typeContent = DataInputObject {..}, ..}
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
