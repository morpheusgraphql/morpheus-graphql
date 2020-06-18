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
    selectType,
    TypeSystemContext (..),
    constraintInterface,
    inField,
    inType,
    inArgument,
    inInterface,
    Field (..),
    Interface (..),
    WithScope' (..),
    renderField,
  )
where

import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
    withReaderT,
  )
--import Data.Morpheus.Error.Document.Interface (unknownInterface)
import Data.Morpheus.Error.Utils (globalErrorMessage)
-- MORPHEUS

import Data.Morpheus.Internal.Utils
  ( Failure (..),
    fromElems,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldName,
    FieldsDefinition,
    GQLError (..),
    GQLErrors,
    Message,
    OUT,
    Position (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( InputContext,
    Scope (..),
    ScopeKind (..),
    Validator (..),
    WithInput,
    WithSchema (..),
    WithScope,
    WithScope' (..),
    renderField,
    withContext,
  )
import Data.Semigroup
  ( (<>),
    Semigroup (..),
  )

data Prop = Prop
  { propName :: FieldName,
    propTypeName :: TypeName
  }
  deriving (Show)

data TypeSystemContext c = TypeSystemContext
  { types :: [TypeDefinition ANY],
    local :: c
  }
  deriving (Show)

instance WithScope' (TypeSystemContext a) where
  getScope' _ =
    Scope
      { position = Position {line = 0, column = 0},
        typename = "TODO:",
        kind = TYPE
      }
  setScope' _ = id --TODO:

selectType :: TypeName -> SchemaValidator ctx (TypeDefinition ANY)
selectType name =
  asks types
    >>= selectBy err name
  where
    -- TODO: use same types
    err = globalErrorMessage $ "Unknown Type " <> msg name <> "."

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

withLocalContext :: (a -> b) -> Validator (TypeSystemContext b) v -> Validator (TypeSystemContext a) v
withLocalContext = withContext . updateLocal

updateLocal :: (a -> b) -> TypeSystemContext a -> TypeSystemContext b
updateLocal f ctx = ctx {local = f (local ctx)}

type SchemaValidator c = Validator (TypeSystemContext c)

constraintInterface :: TypeDefinition ANY -> SchemaValidator ctx (TypeName, FieldsDefinition OUT)
constraintInterface
  TypeDefinition
    { typeName,
      typeContent = DataInterface fields
    } = pure (typeName, fields)
constraintInterface TypeDefinition {typeName} =
  failure $ globalErrorMessage $ "type " <> msg typeName <> " must be an interface"

instance WithSchema (Validator (TypeSystemContext ())) where
  askSchema = asks types >>= fromElems
