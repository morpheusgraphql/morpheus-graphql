{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( SchemaValidator (..),
    runSchemaValidator,
    withContext,
    selectType,
    Context (..),
    constraintInterface,
    inField,
    inType,
    inArgument,
    inInterface,
    FieldArg (..),
    Interface (..),
  )
where

import Control.Monad.Reader (MonadReader, asks, runReaderT)
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
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
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

data Context c = Context
  { types :: [TypeDefinition ANY],
    local :: c
  }
  deriving (Show)

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
inType name = withContext (const name)

inInterface ::
  TypeName ->
  SchemaValidator Interface v ->
  SchemaValidator TypeName v
inInterface interfaceName = withContext (Interface interfaceName)

inField ::
  FieldName ->
  SchemaValidator (t, FieldName) v ->
  SchemaValidator t v
inField fname = withContext (,fname)

inArgument ::
  FieldName ->
  SchemaValidator (t, FieldArg) v ->
  SchemaValidator (t, FieldName) v
inArgument aname = withContext (\(t1, f1) -> (t1, FieldArg f1 aname))

data Interface = Interface
  { interfaceName :: TypeName,
    typeName :: TypeName
  }

data FieldArg = FieldArg
  { fieldName :: FieldName,
    fieldArg :: FieldName
  }

updateLocal :: (a -> b) -> Context a -> Context b
updateLocal f ctx = ctx {local = f (local ctx)}

withContext ::
  (a -> b) ->
  SchemaValidator b v ->
  SchemaValidator a v
withContext f =
  SchemaValidator
    . withReaderT
      (updateLocal f)
    . _runValidator

runSchemaValidator :: SchemaValidator ctx a -> Context ctx -> Eventless a
runSchemaValidator (SchemaValidator x) = runReaderT x

newtype SchemaValidator c a = SchemaValidator
  { _runValidator ::
      ReaderT
        ( Context
            c
        )
        Eventless
        a
  }
  deriving
    ( Functor,
      Applicative,
      MonadReader (Context c),
      Monad
    )

constraintInterface :: TypeDefinition ANY -> SchemaValidator ctx (TypeName, FieldsDefinition OUT)
constraintInterface
  TypeDefinition
    { typeName,
      typeContent = DataInterface fields
    } = pure (typeName, fields)
constraintInterface TypeDefinition {typeName} =
  failure $ globalErrorMessage $ "type " <> msg typeName <> " must be an interface"

instance Failure GQLErrors (SchemaValidator ctx) where
  failure = SchemaValidator . lift . failure
