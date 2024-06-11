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
    GQLError,
    IMPLEMENTABLE,
    IN,
    OBJECT,
    OUT,
    Operation (..),
    OrdMap,
    TRUE,
    ToOBJECT,
    Token,
    TypeName,
    TypeRef,
    UnionMember (..),
    VALID,
    fromAny,
    fromCategory,
    getOperationDataType,
    internal,
    msg,
    typeConName,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
import Data.Morpheus.Types.Internal.Validation.Validator
  ( SelectionValidator,
    ValidatorContext (schema),
  )
import Relude

askType ::
  (Constraints m c cat s ctx) =>
  Typed cat s TypeRef ->
  m (TypeDefinition cat s)
askType = untyped (__askType . typeConName)

askType2 ::
  (Constraints m c cat s ctx) =>
  Typed cat s TypeName ->
  m (TypeDefinition cat s)
askType2 = untyped __askType

__askType ::
  (Constraints m c cat s ctx) => TypeName -> m (TypeDefinition cat s)
__askType name =
  asks schema
    >>= maybe (throwError (unknownType name)) pure
    . lookupDataType name
    >>= kindConstraint

askTypeMember ::
  (Constraints m c cat s ctx) =>
  UnionMember cat s ->
  m (TypeDefinition (ToOBJECT cat) s)
askTypeMember = askType2 . typed memberName >=> constraintObject

askInterfaceTypes ::
  ( MonadError GQLError m,
    MonadReader (ValidatorContext s ctx) m,
    FromCategory (TypeContent TRUE) ANY IMPLEMENTABLE
  ) =>
  TypeDefinition IMPLEMENTABLE s ->
  m (OrdMap TypeName (TypeDefinition IMPLEMENTABLE s))
askInterfaceTypes typeDef@TypeDefinition {typeName} =
  asks schema
    >>= traverse (validate . fromCategory)
    . possibleInterfaceTypes typeName
    >>= fromElems
    . (typeDef :)
  where
    validate (Just x) = pure x
    validate Nothing = throwError (internal "Invalid interface Types")

type Constraints m c cat s ctx =
  ( MonadError GQLError m,
    Monad m,
    MonadReader (ValidatorContext s ctx) m,
    KindErrors cat,
    FromCategory (TypeContent TRUE) ANY cat
  )

getOperationType :: Operation a -> SelectionValidator (TypeDefinition OBJECT VALID)
getOperationType operation = asks schema >>= getOperationDataType operation

unknownType :: TypeName -> GQLError
unknownType name = internal $ "Type \"" <> msg name <> "\" can't found in Schema."

type KindConstraint f c =
  ( MonadError GQLError f,
    FromCategory TypeDefinition ANY c
  )

_kindConstraint ::
  (KindConstraint f k) =>
  Token ->
  TypeDefinition ANY s ->
  f (TypeDefinition k s)
_kindConstraint err anyType =
  maybe
    (throwError $ violation err (typeName anyType))
    pure
    (fromAny anyType)

class KindErrors c where
  kindConstraint :: (KindConstraint f c) => TypeDefinition ANY s -> f (TypeDefinition c s)
  constraintObject ::
    ( Applicative f,
      MonadError GQLError f
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
  GQLError
violation kind typeName =
  internal
    $ "Type \""
    <> msg typeName
    <> "\" must be an"
    <> msg kind
    <> "."
