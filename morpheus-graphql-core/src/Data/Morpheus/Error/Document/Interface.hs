{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface,
    ImplementsError (..),
    partialImplements,
    Field (..),
    TypeSystemElement (..),
    inArgument,
    inField,
    inInterface,
    inType,
    into,
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
  SchemaValidator TypeSystemElement v ->
  SchemaValidator TypeSystemElement v
inInterface interfaceName = withLocalContext replace
  where
    replace (Interface _ t) = Interface interfaceName t
    replace (Type t) = Interface interfaceName t

inType ::
  TypeName ->
  SchemaValidator TypeSystemElement v ->
  SchemaValidator () v
inType name = withLocalContext (const (Type name))

into ::
  TypeSystemElement ->
  SchemaValidator TypeSystemElement v ->
  SchemaValidator () v
into = withLocalContext . const

inField ::
  FieldName ->
  SchemaValidator Field v ->
  SchemaValidator TypeSystemElement v
inField fname = withLocalContext (Field fname Nothing)

inArgument ::
  FieldName ->
  SchemaValidator Field v ->
  SchemaValidator Field v
inArgument aname = withLocalContext (\field -> field {fieldArgument = Just aname})

data TypeSystemElement
  = Interface
      { interfaceName :: TypeName,
        typeName :: TypeName
      }
  | Type TypeName

data Field = Field
  { fieldName :: FieldName,
    fieldArgument :: Maybe FieldName,
    fieldOf :: TypeSystemElement
  }

data ImplementsError
  = UnexpectedType
      { expectedType :: TypeRef,
        foundType :: TypeRef
      }
  | Missing

partialImplements :: Field -> ImplementsError -> ValidationError
partialImplements (Field fieldName argName (Interface interfaceName typename)) errorType =
  "Interface field " <> maybe "" (const "argument ") argName
    <> renderField interfaceName fieldName argName
    <> detailedMessageGen
      (renderField typename fieldName argName)
      (maybe (msgValidation typename) (const $ renderField typename fieldName Nothing) argName)
      errorType
partialImplements (Field fieldname argName Type {}) errorType = undefined

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
