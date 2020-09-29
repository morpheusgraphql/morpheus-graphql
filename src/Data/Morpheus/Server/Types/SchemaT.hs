{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    closeWith,
    updateSchema,
    insertType,
    setMutation,
    setSubscription,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), foldM)
import Data.Function ((&))
import Data.Functor (Functor (..))
import Data.Morpheus.Error (nameCollisionError)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataFingerprint,
    OBJECT,
    Schema (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    isNotSystemTypeName,
    safeDefineType,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup (Semigroup (..))
import Data.Set (Set, empty)
import Prelude
  ( ($),
    (.),
    Bool (..),
    Eq (..),
    Maybe (..),
    const,
    null,
    otherwise,
    uncurry,
    undefined,
  )

-- Helper Functions
newtype SchemaT a = SchemaT
  { runSchemaT ::
      Eventless
        ( a,
          [(Set DataFingerprint, Schema CONST) -> Eventless (Set DataFingerprint, Schema CONST)]
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

init :: ((Set DataFingerprint, Schema CONST) -> (Set DataFingerprint, Eventless (Schema CONST))) -> SchemaT ()
init f = SchemaT $ pure ((), [f])

setMutation :: TypeDefinition OBJECT CONST -> SchemaT ()
setMutation mut = init (\schema -> pure $ schema {mutation = optionalType mut})

setSubscription :: TypeDefinition OBJECT CONST -> SchemaT ()
setSubscription x = init (\schema -> pure $ schema {subscription = optionalType x})

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

execUpdates :: Monad m => a -> [a -> m a] -> m a
execUpdates = foldM (&)

insertType ::
  TypeDefinition cat CONST ->
  SchemaT ()
insertType dt@TypeDefinition {typeName} =
  updateSchema typeName undefined (const $ pure dt) ()

isTypeDefined :: DataFingerprint -> Set DataFingerprint -> Bool
isTypeDefined _ _ = False

updateSchema ::
  DataFingerprint ->
  (a -> SchemaT (TypeDefinition cat CONST)) ->
  a ->
  SchemaT ()
updateSchema typeFingerprint f x =
  SchemaT $ pure ((), [upLib])
  where
    upLib :: (Set DataFingerprint, Schema CONST) -> Eventless (Set DataFingerprint, Schema CONST)
    upLib (fps, schema)
      | isTypeDefined typeFingerprint fps = pure (fps, schema)
      | otherwise = do
        (tyDef, updater) <- runSchemaT (f x)
        execUpdates lib (safeDefineType tyDef : updater)
