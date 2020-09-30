{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    updateSchema,
    insertType,
    -- setMutation,
    -- setSubscription,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), foldM)
import Data.Foldable (fold, foldl, foldr)
import Data.Function ((&))
import Data.Functor (Functor (..))
import Data.HashMap.Lazy
  ( HashMap,
    insert,
    singleton,
  )
import Data.Hashable (Hashable)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    CONST,
    OBJECT,
    Schema (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Generic)
import Prelude
  ( ($),
    (.),
    Bool (..),
    Eq (..),
    Maybe (..),
    Ord,
    Show,
    String,
    const,
    null,
    otherwise,
    uncurry,
  )

data DataFingerprint = DataFingerprint TypeName [String]
  deriving (Generic, Show, Eq, Ord, Hashable)

internalFingerprint :: TypeName -> [String] -> DataFingerprint
internalFingerprint name = DataFingerprint ("SYSTEM.INTERNAL." <> name)

type MyMap = HashMap DataFingerprint (TypeDefinition ANY CONST)

-- Helper Functions
newtype SchemaT a = SchemaT
  { runSchemaT ::
      Eventless
        ( a,
          [MyMap -> Eventless MyMap]
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
  (SchemaT v1) <*> (SchemaT v2) = SchemaT $ do
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

-- closeWith :: SchemaT (Schema CONST) -> Eventless (Schema CONST)
-- closeWith (SchemaT v) = v >>= uncurry execUpdates

init :: DataFingerprint -> TypeDefinition ANY CONST -> SchemaT ()
init fingerprint ty = SchemaT $ pure ((), [const $ pure (singleton fingerprint ty)])

-- setMutation :: TypeDefinition OBJECT CONST -> SchemaT ()
-- setMutation mut = init (\(fps, schema) -> (fps, schema {mutation = optionalType mut}))

-- setSubscription :: TypeDefinition OBJECT CONST -> SchemaT ()
-- setSubscription x = init (\(fps, schema) -> (fps, pure $ schema {subscription = optionalType x}))

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

execUpdates :: Monad m => a -> [a -> m a] -> m a
execUpdates = foldM (&)

insertType ::
  DataFingerprint ->
  TypeDefinition ANY CONST ->
  SchemaT ()
insertType fp dt = updateSchema fp (const $ pure dt) ()

isTypeDefined :: DataFingerprint -> HashMap DataFingerprint a -> Bool
isTypeDefined _ _ = False

updateSchema ::
  DataFingerprint ->
  (a -> SchemaT (TypeDefinition ANY CONST)) ->
  a ->
  SchemaT ()
updateSchema typeFingerprint f x =
  SchemaT $ pure ((), [upLib])
  where
    upLib :: MyMap -> Eventless MyMap
    upLib lib
      | isTypeDefined typeFingerprint lib = pure lib
      | otherwise = do
        (type', updates) <- runSchemaT (f x)
        execUpdates lib ((pure . insert typeFingerprint type') : updates)
