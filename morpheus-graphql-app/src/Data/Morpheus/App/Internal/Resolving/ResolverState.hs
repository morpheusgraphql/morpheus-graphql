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
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverStateT (..),
    resolverFailureMessage,
    clearStateResolverEvents,
    ResolverState,
    toResolverStateT,
    runResolverStateT,
    runResolverStateM,
    runResolverState,
    runResolverStateValueM,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Reader (mapReaderT)
import Data.Morpheus.Core
  ( Config (..),
    RenderGQL,
    render,
  )
import Data.Morpheus.Internal.Ext
  ( PushEvents (..),
    Result,
    ResultT (..),
    ValidationResult,
    cleanEvents,
  )
import Data.Morpheus.Types.Internal.AST
  ( Operation,
    Schema,
    Selection (..),
    TypeName,
    VALID,
    ValidationError,
    at,
    isInternal,
    msgInternal,
    msgValidation,
  )
import Relude

data ResolverContext = ResolverContext
  { currentSelection :: Selection VALID,
    schema :: Schema VALID,
    operation :: Operation VALID,
    currentTypeName :: TypeName,
    config :: Config
  }
  deriving (Show)

type ResolverState = ResolverStateT () Identity

runResolverStateT :: ResolverStateT e m a -> ResolverContext -> ResultT e m a
runResolverStateT = runReaderT . _runResolverStateT

runResolverStateM :: ResolverStateT e m a -> ResolverContext -> m (Result ValidationError ([e], a))
runResolverStateM res = runResultT . runResolverStateT res

runResolverStateValueM :: Functor m => ResolverStateT e m a -> ResolverContext -> m (Result ValidationError a)
runResolverStateValueM res = fmap (fmap snd) . runResolverStateM res

runResolverState :: ResolverState a -> ResolverContext -> ValidationResult a
runResolverState res = fmap snd . runIdentity . runResolverStateM res

-- Resolver Internal State
newtype ResolverStateT event m a = ResolverStateT
  { _runResolverStateT :: ReaderT ResolverContext (ResultT event m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ResolverContext
    )

instance MonadTrans (ResolverStateT e) where
  lift = ResolverStateT . lift . lift

instance (Monad m) => MonadError ValidationError (ResolverStateT e m) where
  throwError err = do
    ctx <- asks id
    let f = if isInternal err then renderInternalResolverError ctx else resolverFailureMessage ctx
    ResolverStateT
      $ lift
      $ throwError
      $ f err
  catchError (ResolverStateT mx) f = ResolverStateT $ catchError mx (_runResolverStateT . f)

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

-- clear events and starts new resolver with different type of events but with same value
-- use properly. only if you know what you are doing
clearStateResolverEvents :: (Functor m) => ResolverStateT e m a -> ResolverStateT e' m a
clearStateResolverEvents = mapResolverState cleanEvents

resolverFailureMessage :: ResolverContext -> ValidationError -> ValidationError
resolverFailureMessage
  ctx@ResolverContext
    { currentSelection =
        Selection {selectionName, selectionPosition}
    }
  err =
    "Failure on Resolving Field "
      <> msgValidation selectionName
      <> ": "
      <> err
      <> withInternalContext ctx `at` selectionPosition

renderInternalResolverError :: ResolverContext -> ValidationError -> ValidationError
renderInternalResolverError ctx@ResolverContext {currentSelection} err =
  (err <> ". " <> msgInternal (renderContext ctx))
    `at` selectionPosition currentSelection

withInternalContext :: ResolverContext -> ValidationError
withInternalContext ResolverContext {config = Config {debug = False}} = ""
withInternalContext resCTX = renderContext resCTX

renderContext :: ResolverContext -> ValidationError
renderContext
  ResolverContext
    { currentSelection,
      schema,
      operation,
      currentTypeName
    } =
    renderSection "Current Type" currentTypeName
      <> renderSection "Current Selection" currentSelection
      <> renderSection "OperationDefinition" operation
      <> renderSection "SchemaDefinition" schema

renderSection :: RenderGQL a => ValidationError -> a -> ValidationError
renderSection label content =
  "\n\n" <> label <> ":\n" <> line
    <> "\n\n"
    <> msgValidation (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
