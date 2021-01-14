{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
    TypeFingerprint (..),
    toSchema,
    withInput,
    withInterface,
  )
where

import qualified Data.Map as Map
import Data.Morpheus.App.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    IN,
    OBJECT,
    OUT,
    Schema (..),
    TypeCategory (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    defineSchemaWith,
    msg,
    toAny,
  )
import GHC.Fingerprint.Type (Fingerprint)
import Relude hiding (empty)

data TypeFingerprint
  = TypeableFingerprint TypeCategory [Fingerprint]
  | InternalFingerprint TypeName
  | CustomFingerprint TypeName
  deriving
    ( Generic,
      Show,
      Eq,
      Ord
    )

type MyMap = Map TypeFingerprint (TypeDefinition ANY CONST)

-- Helper Functions
newtype SchemaT (cat :: TypeCategory) a = SchemaT
  { runSchemaT ::
      Eventless
        ( a,
          [MyMap -> Eventless MyMap]
        )
  }
  deriving (Functor)

instance
  Failure err Eventless =>
  Failure err (SchemaT c)
  where
  failure = SchemaT . failure

instance Applicative (SchemaT c) where
  pure = SchemaT . pure . (,[])
  (SchemaT v1) <*> (SchemaT v2) = SchemaT $ do
    (f, u1) <- v1
    (a, u2) <- v2
    pure (f a, u1 <> u2)

instance Monad (SchemaT c) where
  return = pure
  (SchemaT v1) >>= f =
    SchemaT $ do
      (x, up1) <- v1
      (y, up2) <- runSchemaT (f x)
      pure (y, up1 <> up2)

toSchema ::
  SchemaT
    c
    ( TypeDefinition OBJECT CONST,
      TypeDefinition OBJECT CONST,
      TypeDefinition OBJECT CONST
    ) ->
  Eventless (Schema CONST)
toSchema (SchemaT v) = do
  ((q, m, s), typeDefs) <- v
  types <-
    execUpdates Map.empty typeDefs
      >>= checkTypeCollisions . Map.toList
  defineSchemaWith types (optionalType q, optionalType m, optionalType s)

withInput :: SchemaT IN a -> SchemaT OUT a
withInput (SchemaT x) = SchemaT x

withInterface :: SchemaT OUT a -> SchemaT ct a
withInterface (SchemaT x) = SchemaT x

checkTypeCollisions :: [(TypeFingerprint, TypeDefinition k a)] -> Eventless [TypeDefinition k a]
checkTypeCollisions = fmap Map.elems . foldlM collectTypes Map.empty
  where
    collectTypes :: Map (TypeName, TypeFingerprint) (TypeDefinition k a) -> (TypeFingerprint, TypeDefinition k a) -> Eventless (Map (TypeName, TypeFingerprint) (TypeDefinition k a))
    collectTypes accum (fp, typ) = maybe addType (handleCollision typ) (key `Map.lookup` accum)
      where
        addType = pure $ Map.insert key typ accum
        key = (typeName typ, withSameCategory fp)
        handleCollision t1@TypeDefinition {typeContent = DataEnum {}} t2 | t1 == t2 = pure accum
        handleCollision TypeDefinition {typeContent = DataScalar {}} TypeDefinition {typeContent = DataScalar {}} = pure accum
        handleCollision TypeDefinition {typeName = name1} _ = failureRequirePrefix name1

failureRequirePrefix :: TypeName -> Eventless b
failureRequirePrefix typename =
  failure
    $ globalErrorMessage
    $ "It appears that the Haskell type "
      <> msg typename
      <> " was used as both input and output type, which is not allowed by GraphQL specifications."
      <> "\n\n "
      <> "If you supply \"typeNameModifier\" in \"GQLType.typeOptions\", "
      <> "you can override the default type names for "
      <> msg typename
      <> " to solve this problem."

withSameCategory :: TypeFingerprint -> TypeFingerprint
withSameCategory (TypeableFingerprint _ xs) = TypeableFingerprint OUT xs
withSameCategory x = x

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

execUpdates :: Monad m => a -> [a -> m a] -> m a
execUpdates = foldlM (&)

insertType :: TypeDefinition cat CONST -> SchemaT cat' ()
insertType dt = updateSchema (CustomFingerprint (typeName dt)) (const $ pure dt) ()

updateSchema ::
  TypeFingerprint ->
  (a -> SchemaT cat' (TypeDefinition cat CONST)) ->
  a ->
  SchemaT cat' ()
updateSchema InternalFingerprint {} _ _ = SchemaT $ pure ((), [])
updateSchema fingerprint f x =
  SchemaT $ pure ((), [upLib])
  where
    upLib :: MyMap -> Eventless MyMap
    upLib lib
      | Map.member fingerprint lib = pure lib
      | otherwise = do
        (type', updates) <- runSchemaT (f x)
        execUpdates lib ((pure . Map.insert fingerprint (toAny type')) : updates)
