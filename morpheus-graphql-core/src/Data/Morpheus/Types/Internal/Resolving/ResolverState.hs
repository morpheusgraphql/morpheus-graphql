{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving.ResolverState
  ( Context (..),
    ResolverStateT (..),
    resolverFailureMessage,
    clearStateResolverEvents,
    ResolverState,
    toResolverStateT,
    runResolverStateT,
    runResolverStateM,
    runResolverState,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
    mapReaderT,
  )
import Data.Functor (Functor (..))
import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError (..),
    GQLErrors,
    InternalError,
    Message,
    Operation,
    Schema,
    Selection (..),
    TypeName,
    VALID,
    msg,
  )
import Data.Morpheus.Types.Internal.Config (Config (..))
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Eventless,
    Failure (..),
    PushEvents (..),
    Result,
    ResultT (..),
    cleanEvents,
  )
import Data.Semigroup
  ( Semigroup (..),
  )
import Prelude
  ( ($),
    (.),
    Int,
    Show (..),
    id,
  )

data Context = Context
  { currentSelection :: Selection VALID,
    schema :: Schema VALID,
    operation :: Operation VALID,
    currentTypeName :: TypeName,
    config :: Config
  }
  deriving (Show)

type ResolverState = ResolverStateT () Identity

runResolverStateT :: ResolverStateT e m a -> Context -> ResultT e m a
runResolverStateT = runReaderT . _runResolverStateT

runResolverStateM :: ResolverStateT e m a -> Context -> m (Result e a)
runResolverStateM res = runResultT . runResolverStateT res

runResolverState :: ResolverState a -> Context -> Eventless a
runResolverState res = runIdentity . runResolverStateM res

-- Resolver Internal State
newtype ResolverStateT event m a = ResolverStateT
  { _runResolverStateT :: ReaderT Context (ResultT event m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Context
    )

instance MonadTrans (ResolverStateT e) where
  lift = ResolverStateT . lift . lift

instance (Monad m) => Failure Message (ResolverStateT e m) where
  failure message = do
    selection <- asks currentSelection
    failure [resolverFailureMessage selection message]

instance (Monad m) => Failure InternalError (ResolverStateT e m) where
  failure message = do
    ctx <- asks id
    failure [renderInternalResolverError ctx message]

instance (Monad m) => Failure GQLErrors (ResolverStateT e m) where
  failure = ResolverStateT . lift . failure

instance (Monad m) => PushEvents e (ResolverStateT e m) where
  pushEvents = ResolverStateT . lift . pushEvents

mapResolverState ::
  ( ResultT e m a ->
    ResultT e' m' a'
  ) ->
  ResolverStateT e m a ->
  ResolverStateT e' m' a'
mapResolverState f (ResolverStateT x) = ResolverStateT (mapReaderT f x)

toResolverStateT ::
  Applicative m =>
  ResolverState a ->
  ResolverStateT e m a
toResolverStateT = mapResolverState injectResult

injectResult ::
  (Applicative m) =>
  ResultT () Identity a ->
  ResultT e m a
injectResult (ResultT (Identity x)) =
  cleanEvents $ ResultT (pure x)

-- clear evets and starts new resolver with diferenct type of events but with same value
-- use properly. only if you know what you are doing
clearStateResolverEvents :: (Functor m) => ResolverStateT e m a -> ResolverStateT e' m a
clearStateResolverEvents = mapResolverState cleanEvents

resolverFailureMessage :: Selection VALID -> Message -> GQLError
resolverFailureMessage Selection {selectionName, selectionPosition} message =
  GQLError
    { message = "Failure on Resolving Field " <> msg selectionName <> ": " <> message,
      locations = [selectionPosition]
    }

renderInternalResolverError :: Context -> InternalError -> GQLError
renderInternalResolverError ctx@Context {currentSelection} message =
  GQLError
    { message = msg message <> ". " <> renderContext ctx,
      locations = [selectionPosition currentSelection]
    }

renderContext :: Context -> Message
renderContext
  Context
    { currentSelection,
      schema,
      operation,
      currentTypeName
    } =
    renderSection "Current Type" currentTypeName
      <> renderSection "Current Selection" currentSelection
      <> renderSection "OperationDefinition" operation
      <> renderSection "SchemaDefinition" schema

renderSection :: RenderGQL a => Message -> a -> Message
renderSection label content =
  "\n\n" <> label <> ":\n" <> line
    <> "\n\n"
    <> msg (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
