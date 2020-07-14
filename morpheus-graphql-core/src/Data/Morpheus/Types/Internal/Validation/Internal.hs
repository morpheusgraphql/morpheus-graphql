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
  ( askFieldType,
    askTypeMember,
    askInputMember,
    getOperationType,
    askInputFieldType,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative, pure)
import Control.Monad (Monad ((>>=)))
import Data.Functor ((<$>))
import Data.Maybe (maybe)
import Data.Morpheus.Error.Operation
  ( mutationIsNotDefined,
    subscriptionIsNotDefined,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    selectBy,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldsDefinition,
    FromAny,
    GQLErrors,
    IN,
    InternalError,
    OUT,
    Operation,
    Operation (..),
    OperationType (..),
    Schema (..),
    Token,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    UnionMember (..),
    VALID,
    fromAny,
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
    fst,
  )

askFieldType ::
  TypeRef ->
  SelectionValidator (TypeDefinition OUT VALID)
askFieldType TypeRef {typeConName} =
  askSchema
    >>= selectBy (unknownType typeConName) typeConName
    >>= kindConstraint

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
    >>= kindConstraint

askTypeMember ::
  UnionMember OUT s ->
  SelectionValidator
    ( TypeDefinition OUT VALID,
      FieldsDefinition OUT VALID
    )
askTypeMember UnionMember {memberName} =
  askSchema
    >>= selectOr notFound pure memberName
    >>= kindConstraint
    >>= constraintObject
  where
    notFound = failure (unknownType memberName)

askInputMember ::
  ( GetWith c (Schema s),
    Failure InternalError (m c),
    Monad (m c),
    MonadContext m c
  ) =>
  TypeName ->
  m c (TypeDefinition IN s)
askInputMember name =
  fst
    <$> ( askSchema
            >>= selectOr notFound pure name
            >>= kindConstraint
            >>= constraintObject
        )
  where
    notFound = failure (unknownType name)

getOperationType :: Operation a -> SelectionValidator (TypeDefinition OUT VALID, FieldsDefinition OUT VALID)
getOperationType operation =
  askSchema
    >>= getOperationDataType operation
    >>= constraintObject

getOperationDataType :: Failure GQLErrors m => Operation s -> Schema VALID -> m (TypeDefinition OUT VALID)
getOperationDataType Operation {operationType = Query} lib = pure (query lib)
getOperationDataType Operation {operationType = Mutation, operationPosition} lib =
  maybe (failure $ mutationIsNotDefined operationPosition) pure (mutation lib)
getOperationDataType Operation {operationType = Subscription, operationPosition} lib =
  maybe (failure $ subscriptionIsNotDefined operationPosition) pure (subscription lib)

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