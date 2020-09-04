{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    closeWith,
    concatSchemaT,
    updateSchema,
    updateExperimental,
    injectUpdate,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), foldM)
import Data.Function ((&))
import Data.Functor (Functor (..))
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataFingerprint,
    GQLError,
    Schema,
    TypeDefinition,
    TypeName (..),
    ValidationErrors,
    isNotSystemTypeName,
    isTypeDefined,
    safeDefineType,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Prelude
  ( ($),
    (.),
    Eq (..),
    Maybe (..),
    map,
    otherwise,
    undefined,
  )

-- Helper Functions
data SchemaT a = SchemaT
  { typeUpdates :: Schema CONST -> Eventless (Schema CONST),
    currentValue :: Eventless a
  }

instance Failure [GQLError] SchemaT

instance Functor SchemaT

instance Applicative SchemaT

instance Monad SchemaT

injectUpdate :: (Schema CONST -> Eventless (Schema CONST)) -> SchemaT ()
injectUpdate f = SchemaT f $ pure ()

closeWith :: SchemaT (Schema CONST) -> Eventless (Schema CONST)
closeWith SchemaT {typeUpdates, currentValue} = currentValue >>= typeUpdates

concatSchemaT :: [SchemaT ()] -> SchemaT ()
concatSchemaT = undefined

execUpdates :: Monad m => a -> [a -> m a] -> m a
execUpdates = foldM (&)

updateExperimental ::
  SchemaT (TypeDefinition cat CONST) ->
  SchemaT ()
updateExperimental x = undefined

updateSchema ::
  TypeName ->
  DataFingerprint ->
  [SchemaT ()] ->
  (a -> TypeDefinition cat CONST) ->
  a ->
  SchemaT ()
updateSchema name fingerprint stack f x
  | isNotSystemTypeName name = SchemaT upLib (pure ())
  | otherwise = concatSchemaT stack
  where
    updates = map typeUpdates stack
    upLib lib = case isTypeDefined name lib of
      Nothing -> execUpdates lib (safeDefineType (f x) : updates)
      Just fingerprint'
        | fingerprint' == fingerprint -> pure lib
        -- throw error if 2 different types has same name
        | otherwise -> failure (["bla"] :: ValidationErrors)
