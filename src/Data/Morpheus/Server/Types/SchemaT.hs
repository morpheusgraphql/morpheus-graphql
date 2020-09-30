{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
    updateSchema,
    insertType,
    TypeFingerprint (..),
    toSchema,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), foldM)
import Data.Function ((&))
import Data.Functor ((<$>), Functor (..))
import Data.HashMap.Lazy
  ( HashMap,
    elems,
    empty,
    insert,
    member,
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
    defineSchemaWith,
    toAny,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Generic)
import Prelude
  ( ($),
    (.),
    Eq (..),
    Maybe (..),
    Ord,
    Show,
    String,
    const,
    null,
    otherwise,
  )

data TypeFingerprint
  = TypeFingerprint TypeName [String]
  | InternalFingerprint TypeName
  | CustomFingerprint TypeName TypeFingerprint
  deriving
    ( Generic,
      Show,
      Eq,
      Ord,
      Hashable
    )

type MyMap = HashMap TypeFingerprint (TypeDefinition ANY CONST)

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

toSchema ::
  SchemaT
    ( TypeDefinition OBJECT CONST,
      TypeDefinition OBJECT CONST,
      TypeDefinition OBJECT CONST
    ) ->
  Eventless (Schema CONST)
toSchema (SchemaT v) = do
  ((q, m, s), typeDefs) <- v
  types <- elems <$> execUpdates empty typeDefs
  defineSchemaWith types (optionalType q, optionalType m, optionalType s)

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

execUpdates :: Monad m => a -> [a -> m a] -> m a
execUpdates = foldM (&)

insertType ::
  TypeFingerprint ->
  TypeDefinition cat CONST ->
  SchemaT ()
insertType fp dt = updateSchema (CustomFingerprint (typeName dt) fp) (const $ pure dt) ()

updateSchema ::
  TypeFingerprint ->
  (a -> SchemaT (TypeDefinition cat CONST)) ->
  a ->
  SchemaT ()
updateSchema InternalFingerprint {} _ _ = SchemaT $ pure ((), [])
updateSchema fingerprint f x =
  SchemaT $ pure ((), [upLib])
  where
    upLib :: MyMap -> Eventless MyMap
    upLib lib
      | member fingerprint lib = pure lib
      | otherwise = do
        (type', updates) <- runSchemaT (f x)
        execUpdates lib ((pure . insert fingerprint (toAny type')) : updates)
