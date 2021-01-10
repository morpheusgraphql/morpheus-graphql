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

module Data.Morpheus.Types.Internal.Resolving.ResolverState
  ( ResolverContext (..),
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

import Control.Monad.Trans.Reader (mapReaderT)
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    renderGQL,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError (..),
    GQLErrors,
    InternalError (..),
    Message,
    Operation,
    Schema,
    Selection (..),
    TypeName,
    VALID,
    ValidationError (..),
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

runResolverStateM :: ResolverStateT e m a -> ResolverContext -> m (Result e a)
runResolverStateM res = runResultT . runResolverStateT res

runResolverState :: ResolverState a -> ResolverContext -> Eventless a
runResolverState res = runIdentity . runResolverStateM res

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

instance (Monad m) => Failure Message (ResolverStateT e m) where
  failure message = do
    cxt <- asks id
    failure [resolverFailureMessage cxt message]

instance (Monad m) => Failure InternalError (ResolverStateT e m) where
  failure message = do
    ctx <- asks id
    failure [renderInternalResolverError ctx message]

instance Monad m => Failure [ValidationError] (ResolverStateT e m) where
  failure messages = do
    ctx <- asks id
    failure $ fmap (resolverFailureMessage ctx . validationMessage) messages

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

resolverFailureMessage :: ResolverContext -> Message -> GQLError
resolverFailureMessage
  ctx@ResolverContext
    { currentSelection =
        Selection {selectionName, selectionPosition}
    }
  message =
    GQLError
      { message =
          "Failure on Resolving Field "
            <> msg selectionName
            <> ": "
            <> message
            <> withInternalContext ctx,
        locations = [selectionPosition]
      }

renderInternalResolverError :: ResolverContext -> InternalError -> GQLError
renderInternalResolverError ctx@ResolverContext {currentSelection} message =
  GQLError
    { message = msg message <> ". " <> renderContext ctx,
      locations = [selectionPosition currentSelection]
    }

withInternalContext :: ResolverContext -> Message
withInternalContext ResolverContext {config = Config {debug = False}} = ""
withInternalContext resCTX = renderContext resCTX

renderContext :: ResolverContext -> Message
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

renderSection :: RenderGQL a => Message -> a -> Message
renderSection label content =
  "\n\n" <> label <> ":\n" <> line
    <> "\n\n"
    <> msg (renderGQL content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
