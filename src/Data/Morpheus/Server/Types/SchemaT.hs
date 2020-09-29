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
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    CONST,
    DataFingerprint,
    OBJECT,
    Schema (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    isNotSystemTypeName,
    safeDefineType,
    toAny,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup (Semigroup (..))
import Data.Set (Set, empty, insert)
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

type Value = (Set DataFingerprint, [TypeDefinition ANY CONST])

-- Helper Functions
newtype SchemaT a = SchemaT
  { runSchemaT :: (a, [Value -> Value])
  }
  deriving (Functor)

instance
  Failure err Eventless =>
  Failure err SchemaT
  where
  failure = SchemaT . failure

instance Applicative SchemaT where
  pure = SchemaT . (,[])
  (SchemaT (f, u1)) <*> (SchemaT (a, u2)) =
    SchemaT (f a, u1 <> u2)

instance Monad SchemaT where
  return = pure
  (SchemaT (x, up1)) >>= f =
    SchemaT $
      let (y, up2) = runSchemaT (f x)
       in (y, up1 <> up2)

closeWith :: SchemaT (Schema CONST) -> Eventless (Schema CONST)
closeWith (SchemaT v) = v >>= uncurry execUpdates

init :: (Value -> Value) -> SchemaT ()
init f = SchemaT ((), [f])

setMutation :: TypeDefinition OBJECT CONST -> SchemaT ()
setMutation mut = init (\(fps, schema) -> (fps, schema {mutation = optionalType mut}))

setSubscription :: TypeDefinition OBJECT CONST -> SchemaT ()
setSubscription x = init (\(fps, schema) -> (fps, pure $ schema {subscription = optionalType x}))

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
  (a -> SchemaT (TypeDefinition ANY CONST)) ->
  a ->
  SchemaT ()
updateSchema typeFingerprint f x =
  SchemaT $ pure ((), [upLib])
  where
    upLib :: Value -> Value
    upLib (fps, typeDef)
      | isTypeDefined typeFingerprint fps = (fps, typeDef)
      | otherwise = do
        (tyDef, updater) <- runSchemaT (f x)
        (insert typeFingerprint fps, tyDef : updater)
