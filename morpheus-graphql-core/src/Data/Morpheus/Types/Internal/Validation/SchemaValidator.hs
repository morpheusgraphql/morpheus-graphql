{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( SchemaValidator,
    TypeSystemContext (..),
    constraintInterface,
    inField,
    inType,
    inArgument,
    inInterface,
    Field (..),
    Interface (..),
    renderField,
  )
where

--import Data.Morpheus.Error.Document.Interface (unknownInterface)

-- MORPHEUS

import Control.Applicative (pure)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    FieldName,
    FieldsDefinition,
    OUT,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    ValidationError,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( Validator (..),
    renderField,
    withContext,
  )
import Data.Semigroup
  ( (<>),
    Semigroup (..),
  )
import Prelude
  ( (.),
    Show (..),
    const,
  )

newtype TypeSystemContext c = TypeSystemContext
  {local :: c}
  deriving (Show)

inType ::
  TypeName ->
  SchemaValidator TypeName v ->
  SchemaValidator () v
inType name = withLocalContext (const name)

inInterface ::
  TypeName ->
  SchemaValidator Interface v ->
  SchemaValidator TypeName v
inInterface interfaceName = withLocalContext (Interface interfaceName)

inField ::
  FieldName ->
  SchemaValidator (t, FieldName) v ->
  SchemaValidator t v
inField fname = withLocalContext (,fname)

inArgument ::
  FieldName ->
  SchemaValidator (t, Field) v ->
  SchemaValidator (t, FieldName) v
inArgument aname = withLocalContext (\(t1, f1) -> (t1, Field f1 aname))

data Interface = Interface
  { interfaceName :: TypeName,
    typeName :: TypeName
  }

data Field = Field
  { fieldName :: FieldName,
    fieldArgument :: FieldName
  }

withLocalContext :: (a -> b) -> SchemaValidator b v -> SchemaValidator a v
withLocalContext = withContext . updateLocal

updateLocal :: (a -> b) -> TypeSystemContext a -> TypeSystemContext b
updateLocal f ctx = ctx {local = f (local ctx)}

type SchemaValidator c = Validator CONST (TypeSystemContext c)

constraintInterface ::
  TypeDefinition ANY CONST ->
  SchemaValidator ctx (TypeName, FieldsDefinition OUT CONST)
constraintInterface
  TypeDefinition
    { typeName,
      typeContent = DataInterface fields
    } = pure (typeName, fields)
constraintInterface TypeDefinition {typeName} =
  failure ["type " <> msgValidation typeName <> " must be an interface" :: ValidationError]
