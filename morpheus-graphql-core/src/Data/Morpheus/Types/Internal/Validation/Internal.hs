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

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.Internal.Utils
  ( fromElems,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FromCategory,
    IMPLEMENTABLE,
    IN,
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
    ValidationError,
    fromAny,
    fromCategory,
    getOperationDataType,
    internal,
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
    >>= maybe (throwError (unknownType name)) pure . lookupDataType name
    >>= kindConstraint

askTypeMember ::
  Constraints m c cat s =>
  UnionMember cat s ->
  m c (TypeDefinition (ToOBJECT cat) s)
askTypeMember = askType2 . typed memberName >=> constraintObject

askInterfaceTypes ::
  ( MonadError ValidationError (m c),
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
    validate Nothing = throwError (internal "Invalid interface Types")

type Constraints m c cat s =
  ( MonadError ValidationError (m c),
    Monad (m c),
    MonadContext m s c,
    KindErrors cat,
    FromCategory (TypeContent TRUE) ANY cat
  )

getOperationType :: Operation a -> SelectionValidator (TypeDefinition OBJECT VALID)
getOperationType operation = askSchema >>= getOperationDataType operation

unknownType :: TypeName -> ValidationError
unknownType name = internal $ "Type \"" <> msgInternal name <> "\" can't found in Schema."

type KindConstraint f c =
  ( MonadError ValidationError f,
    FromCategory TypeDefinition ANY c
  )

_kindConstraint ::
  KindConstraint f k =>
  Token ->
  TypeDefinition ANY s ->
  f (TypeDefinition k s)
_kindConstraint err anyType =
  maybe
    (throwError $ violation err (typeName anyType))
    pure
    (fromAny anyType)

class KindErrors c where
  kindConstraint :: KindConstraint f c => TypeDefinition ANY s -> f (TypeDefinition c s)
  constraintObject ::
    ( Applicative f,
      MonadError ValidationError f
    ) =>
    TypeDefinition c s ->
    f (TypeDefinition (ToOBJECT c) s)

instance KindErrors IN where
  kindConstraint = _kindConstraint "input type"
  constraintObject TypeDefinition {typeContent = DataInputObject {..}, ..} = pure TypeDefinition {typeContent = DataInputObject {..}, ..}
  constraintObject TypeDefinition {typeName} = throwError (violation "input object" typeName)

instance KindErrors OUT where
  kindConstraint = _kindConstraint "output type"
  constraintObject TypeDefinition {typeContent = DataObject {..}, ..} = pure TypeDefinition {typeContent = DataObject {..}, ..}
  constraintObject TypeDefinition {typeName} = throwError (violation "object" typeName)

violation ::
  Token ->
  TypeName ->
  ValidationError
violation kind typeName =
  internal $
    "Type \"" <> msgInternal typeName
      <> "\" must be an"
      <> msgInternal kind
      <> "."
