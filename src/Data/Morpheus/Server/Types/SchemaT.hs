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
    TypeFingerprint (..),
    toSchema,
  )
where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    CONST,
    OBJECT,
    Schema (..),
    TypeCategory (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    defineSchemaWith,
    msg,
    toAny,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
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
  types <-
    execUpdates Map.empty typeDefs
      >>= checkTypeColisions . Map.toList
  defineSchemaWith types (optionalType q, optionalType m, optionalType s)

checkTypeColisions :: [(TypeFingerprint, TypeDefinition k a)] -> Eventless [TypeDefinition k a]
checkTypeColisions = fmap Map.elems . foldM collectTypes Map.empty
  where
    collectTypes :: Map (TypeName, TypeFingerprint) (TypeDefinition k a) -> (TypeFingerprint, TypeDefinition k a) -> Eventless (Map (TypeName, TypeFingerprint) (TypeDefinition k a))
    collectTypes accum (fp, typ) = maybe addType (hanldeCollision typ) (key `Map.lookup` accum)
      where
        addType = pure $ Map.insert key typ accum
        key = (typeName typ, withSameCategory fp)
        hanldeCollision t1@TypeDefinition {typeContent = DataEnum {}} t2 | t1 == t2 = pure accum
        hanldeCollision TypeDefinition {typeContent = DataScalar {}} TypeDefinition {typeContent = DataScalar {}} = pure accum
        hanldeCollision TypeDefinition {typeName = name1} _ = failureRequirePrefix name1

failureRequirePrefix :: TypeName -> Eventless b
failureRequirePrefix typename =
  failure
    $ globalErrorMessage
    $ "It appears that the Haskell type "
      <> msg typename
      <> "was used both as input and output type, which is not allowed by GraphQL specifications."
      <> "\n\n "
      <> "If you enable \"{ prefixInputType = True }\" in \"GQLType.typeOptions\", "
      <> "the compiler can generate a new input type "
      <> msg ("Input" <> typename)
      <> " to solve this problem."

--handle x = x
withSameCategory :: TypeFingerprint -> TypeFingerprint
withSameCategory (TypeableFingerprint _ xs) = TypeableFingerprint OUT xs
withSameCategory x = x

-- (typ@TypeDefinition {typeContent = DataEnum {}}) | typ `elem` accum = accum
-- collectTypes accum typ = accum <> [(fing, typ)]

optionalType :: TypeDefinition OBJECT CONST -> Maybe (TypeDefinition OBJECT CONST)
optionalType td@TypeDefinition {typeContent = DataObject {objectFields}}
  | null objectFields = Nothing
  | otherwise = Just td

execUpdates :: Monad m => a -> [a -> m a] -> m a
execUpdates = foldM (&)

insertType :: TypeDefinition cat CONST -> SchemaT ()
insertType dt = updateSchema (CustomFingerprint (typeName dt)) (const $ pure dt) ()

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
      | Map.member fingerprint lib = pure lib
      | otherwise = do
        (type', updates) <- runSchemaT (f x)
        execUpdates lib ((pure . Map.insert fingerprint (toAny type')) : updates)
