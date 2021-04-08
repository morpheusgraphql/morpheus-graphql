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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( SchemaValidator,
    TypeSystemContext (..),
    constraintInterface,
    renderField,
    withLocalContext,
  )
where

import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
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
import Relude hiding (local)

newtype TypeSystemContext c = TypeSystemContext
  {local :: c}
  deriving (Show)

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
