{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( SchemaValidator (..),
    runSchemaValidator,
    withContext,
    selectType,
    Context (..),
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
    GQLError (..),
    GQLErrors,
    Message,
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

data Context = Context
  { types :: [TypeDefinition ANY],
    currentTypeName :: TypeName,
    currentField :: [Prop]
  }
  deriving (Show)

selectType :: TypeName -> SchemaValidator (TypeDefinition ANY)
selectType name =
  asks types
    >>= selectBy err name
  where
    -- TODO: use same types
    err = globalErrorMessage $ "Unknown Type " <> msg name <> "."

withContext ::
  (Context -> Context) ->
  SchemaValidator a ->
  SchemaValidator a
withContext f = Validator . withReaderT f . _runValidator

runSchemaValidator :: SchemaValidator a -> Context -> Eventless a
runSchemaValidator (Validator x) = runReaderT x

newtype SchemaValidator a = Validator
  { _runValidator ::
      ReaderT
        Context
        Eventless
        a
  }
  deriving
    ( Functor,
      Applicative,
      MonadReader Context,
      Monad
    )

-- can be only used for internal errors
instance Failure Message SchemaValidator where
  failure inputMessage =
    failure
      [ GQLError
          { message = "INTERNAL: " <> inputMessage,
            locations = []
          }
      ]

instance Failure GQLErrors SchemaValidator where
  failure = Validator . lift . failure
