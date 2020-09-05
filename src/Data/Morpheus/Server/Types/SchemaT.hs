{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    closeWith,
    updateSchema,
    updateExperimental,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), Monad (..), foldM)
import Data.Function ((&))
import Data.Functor (($>), (<$>), Functor (..))
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataFingerprint,
    Schema,
    TypeDefinition (..),
    TypeName (..),
    ValidationErrors,
    isNotSystemTypeName,
    isTypeDefined,
    safeDefineType,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup (Semigroup (..))
import Prelude
  ( ($),
    (.),
    Eq (..),
    Maybe (..),
    otherwise,
    uncurry,
  )

-- Helper Functions
newtype SchemaT a = SchemaT
  { runSchemaT ::
      Eventless
        ( a,
          [Schema CONST -> Eventless (Schema CONST)]
        )
  }
  deriving (Functor)

instance
  Failure err Eventless =>
  Failure err SchemaT
  where
  failure = SchemaT . failure

instance Applicative SchemaT where
  pure = SchemaT . pure . (,[])
  (SchemaT v1) <*> (SchemaT v2) =
    SchemaT $ do
      (f, u1) <- v1
      (a, u2) <- v2
      pure (f a, u1 <> u2)

instance Monad SchemaT where
  return = pure
  (SchemaT v1) >>= f =
    SchemaT $ do
      (x, up1) <- v1
      (y, up2) <- runSchemaT (f x)
      pure (y, up1 <> up2)

closeWith :: SchemaT (Schema CONST) -> Eventless (Schema CONST)
closeWith (SchemaT v) = v >>= uncurry execUpdates

execUpdates :: Monad m => a -> [a -> m a] -> m a
execUpdates = foldM (&)

updateExperimental ::
  SchemaT (TypeDefinition cat CONST) ->
  SchemaT ()
updateExperimental (SchemaT v) = SchemaT $ run <$> v
  where
    run (x, y) = ((), runSchema x y)

runSchema ::
  TypeDefinition cat CONST ->
  [Schema CONST -> Eventless (Schema CONST)] ->
  [Schema CONST -> Eventless (Schema CONST)]
runSchema td@TypeDefinition {typeName, typeFingerprint} updater
  | isNotSystemTypeName typeName = [upLib]
  | otherwise = []
  where
    upLib :: Schema CONST -> Eventless (Schema CONST)
    upLib lib = case isTypeDefined typeName lib of
      Nothing -> execUpdates lib (safeDefineType td : updater)
      Just fingerprint'
        | fingerprint' == typeFingerprint -> pure lib
        -- throw error if 2 different types has same name
        | otherwise -> failure (["bla"] :: ValidationErrors)

updateSchema ::
  TypeName ->
  DataFingerprint ->
  SchemaT () ->
  (a -> TypeDefinition cat CONST) ->
  a ->
  SchemaT ()
updateSchema _ _ prev f x =
  updateExperimental $ prev $> f x
