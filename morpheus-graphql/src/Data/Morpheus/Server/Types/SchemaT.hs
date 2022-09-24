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
{-# LANGUAGE RecordWildCards #-}
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
    extendImplements,
    insertDirectiveDefinition,
    outToAny,
  )
where

import Control.Monad.Except (MonadError (..))
import qualified Data.Map as Map
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Types.Directives
import Data.Morpheus.Server.Types.TypeName
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    DirectiveDefinition,
    GQLError,
    IN,
    OBJECT,
    OUT,
    Schema,
    TypeCategory (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    defineDirective,
    defineSchemaWith,
    msg,
    toAny,
  )
import Relude hiding (empty)

data SchemaState where
  SchemaState ::
    { typeDefinitions :: Map TypeFingerprint (TypeDefinition ANY CONST),
      implements :: Map TypeName [TypeName],
      directiveDefinitions ::
        Map
          TypeFingerprint
          ( VisitorFunction,
            DirectiveDefinition CONST
          )
    } ->
    SchemaState

emptyMyMap :: SchemaState
emptyMyMap =
  SchemaState
    { typeDefinitions = Map.empty,
      implements = Map.empty,
      directiveDefinitions = Map.empty
    }

-- Helper Functions
newtype SchemaT (cat :: TypeCategory) a = SchemaT
  { runSchemaT ::
      GQLResult
        ( a,
          [SchemaState -> GQLResult SchemaState]
        )
  }
  deriving (Functor)

instance MonadError GQLError (SchemaT c) where
  throwError = SchemaT . throwError
  catchError (SchemaT mx) f = SchemaT (catchError mx (runSchemaT . f))

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
      Maybe (TypeDefinition OBJECT CONST),
      Maybe (TypeDefinition OBJECT CONST)
    ) ->
  GQLResult (Schema CONST)
toSchema (SchemaT v) = do
  ((q, m, s), typeDefs) <- v
  SchemaState {typeDefinitions, implements, directiveDefinitions} <- execUpdates emptyMyMap typeDefs
  types <- map (insertImplements implements) <$> checkTypeCollisions (Map.toList typeDefinitions)
  schema <- defineSchemaWith types (Just q, m, s)
  foldlM defineDirective schema (fmap snd directiveDefinitions)

insertImplements :: Map TypeName [TypeName] -> TypeDefinition c CONST -> TypeDefinition c CONST
insertImplements x TypeDefinition {typeContent = DataObject {..}, ..} =
  TypeDefinition
    { typeContent =
        DataObject
          { objectImplements = objectImplements <> implements,
            ..
          },
      ..
    }
  where
    implements :: [TypeName]
    implements = Map.findWithDefault [] typeName x
insertImplements _ t = t

withInput :: SchemaT IN a -> SchemaT OUT a
withInput (SchemaT x) = SchemaT x

outToAny :: SchemaT OUT a -> SchemaT k' a
outToAny (SchemaT x) = SchemaT x

checkTypeCollisions :: [(TypeFingerprint, TypeDefinition k a)] -> GQLResult [TypeDefinition k a]
checkTypeCollisions = fmap Map.elems . foldlM collectTypes Map.empty
  where
    collectTypes :: Map (TypeName, TypeFingerprint) (TypeDefinition k a) -> (TypeFingerprint, TypeDefinition k a) -> GQLResult (Map (TypeName, TypeFingerprint) (TypeDefinition k a))
    collectTypes accum (fp, typ) = maybe addType (handleCollision typ) (key `Map.lookup` accum)
      where
        addType = pure $ Map.insert key typ accum
        key = (typeName typ, withSameCategory fp)
        handleCollision t1@TypeDefinition {typeContent = DataEnum {}} t2 | t1 == t2 = pure accum
        handleCollision TypeDefinition {typeContent = DataScalar {}} TypeDefinition {typeContent = DataScalar {}} = pure accum
        handleCollision TypeDefinition {typeName = name1} _ = failureRequirePrefix name1

failureRequirePrefix :: TypeName -> GQLResult b
failureRequirePrefix typename =
  throwError $
    "It appears that the Haskell type "
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
    upLib :: SchemaState -> GQLResult SchemaState
    upLib schema
      | Map.member fingerprint (typeDefinitions schema) = pure schema
      | otherwise = do
          (type', updates) <- runSchemaT (f x)
          execUpdates schema (update type' : updates)
      where
        update t schemaState =
          pure
            schemaState
              { typeDefinitions = Map.insert fingerprint (toAny t) (typeDefinitions schemaState)
              }

insertDirectiveDefinition ::
  TypeFingerprint ->
  (a -> SchemaT cat' (VisitorFunction, DirectiveDefinition CONST)) ->
  a ->
  SchemaT cat' ()
insertDirectiveDefinition InternalFingerprint {} _ _ = SchemaT $ pure ((), [])
insertDirectiveDefinition fingerprint f x =
  SchemaT $ pure ((), [upLib])
  where
    upLib :: SchemaState -> GQLResult SchemaState
    upLib schema
      | Map.member fingerprint (typeDefinitions schema) = pure schema
      | otherwise = do
          (type', updates) <- runSchemaT (f x)
          execUpdates schema (update type' : updates)
      where
        update t schemaState =
          pure
            schemaState
              { directiveDefinitions = Map.insert fingerprint t (directiveDefinitions schemaState)
              }

extendImplements :: TypeName -> [TypeName] -> SchemaT cat' ()
extendImplements interface types = SchemaT $ pure ((), [upLib])
  where
    -- TODO: what happens if interface name collides?
    upLib :: SchemaState -> GQLResult SchemaState
    upLib schema = pure schema {implements = foldr insertInterface (implements schema) types}
    insertInterface :: TypeName -> Map TypeName [TypeName] -> Map TypeName [TypeName]
    insertInterface = Map.alter (Just . (interface :) . fromMaybe [])
