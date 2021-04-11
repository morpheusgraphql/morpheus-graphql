{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface,
    ImplementsError (..),
    partialImplements,
    Field (..),
    TypeEntity (..),
    inArgument,
    inField,
    inInterface,
    inType,
    into,
    PLACE (..),
    INTERFACE,
    TYPE,
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName (..),
    TypeName (..),
    ValidationError,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.AST.Type (TypeRef)
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( SchemaValidator,
    renderField,
    withLocalContext,
  )
import Relude

unknownInterface :: TypeName -> ValidationError
unknownInterface name = "Unknown Interface " <> msgValidation name <> "."

inInterface ::
  TypeName ->
  SchemaValidator (TypeEntity 'INTERFACE) v ->
  SchemaValidator (TypeEntity 'TYPE) v
inInterface name = withLocalContext (\t -> t {interfaceName = OnInterface name})

inType ::
  TypeName ->
  SchemaValidator (TypeEntity 'TYPE) v ->
  SchemaValidator () v
inType name = withLocalContext (const (TypeEntity OnType name))

into ::
  TypeEntity p ->
  SchemaValidator (TypeEntity p) v ->
  SchemaValidator () v
into = withLocalContext . const

inField ::
  FieldName ->
  SchemaValidator (Field p) v ->
  SchemaValidator (TypeEntity p) v
inField fname = withLocalContext (Field fname Nothing)

inArgument ::
  FieldName ->
  SchemaValidator (Field p) v ->
  SchemaValidator (Field p) v
inArgument aname = withLocalContext (\field -> field {fieldArgument = Just aname})

data PLACE = INTERFACE | TYPE

type INTERFACE = 'INTERFACE

type TYPE = 'TYPE

data InterfaceName (p :: PLACE) where
  OnInterface :: TypeName -> InterfaceName 'INTERFACE
  OnType :: InterfaceName 'TYPE

data TypeEntity (p :: PLACE) = TypeEntity
  { interfaceName :: InterfaceName p,
    typeName :: TypeName
  }

data Field p = Field
  { fieldName :: FieldName,
    fieldArgument :: Maybe FieldName,
    fieldOf :: TypeEntity p
  }

data ImplementsError
  = UnexpectedType
      { expectedType :: TypeRef,
        foundType :: TypeRef
      }
  | Missing

partialImplements :: Field 'INTERFACE -> ImplementsError -> ValidationError
partialImplements (Field fieldName argName (TypeEntity (OnInterface interfaceName) typename)) errorType =
  "Interface field " <> maybe "" (const "argument ") argName
    <> renderField interfaceName fieldName argName
    <> detailedMessageGen
      (renderField typename fieldName argName)
      (maybe (msgValidation typename) (const $ renderField typename fieldName Nothing) argName)
      errorType

-- Interface field TestInterface.name expected but User does not provide it.
-- Interface field TestInterface.name expects type String! but User.name is type Int!.
-- Interface field argument TestInterface.name(id:) expected but User.name does not provide it.
-- Interface field argument TestInterface.name(id:) expects type ID but User.name(id:) is type String.

detailedMessageGen :: ValidationError -> ValidationError -> ImplementsError -> ValidationError
detailedMessageGen pl1 _ UnexpectedType {expectedType, foundType} =
  " expects type "
    <> msgValidation expectedType
    <> " but "
    <> pl1
    <> " is type "
    <> msgValidation foundType
    <> "."
detailedMessageGen _ pl2 Missing = " expected but " <> pl2 <> " does not provide it."
