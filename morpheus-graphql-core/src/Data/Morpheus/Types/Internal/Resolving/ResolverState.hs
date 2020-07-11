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
import Data.Functor.Identity (Identity)
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
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Failure (..),
    PushEvents (..),
    ResultT (..),
    cleanEvents,
  )
import Data.Semigroup
  ( Semigroup (..),
  )
import Prelude
  ( ($),
    (.),
    Show (..),
  )

data Context = Context
  { currentSelection :: Selection VALID,
    schema :: Schema VALID,
    operation :: Operation VALID,
    currentTypeName :: TypeName
  }
  deriving (Show)

type ResolverState = ResolverStateT () Identity

-- Resolver Internal State
newtype ResolverStateT event m a = ResolverState
  { runResolverState :: ReaderT Context (ResultT event m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Context
    )

instance MonadTrans (ResolverStateT e) where
  lift = ResolverState . lift . lift

instance (Monad m) => Failure Message (ResolverStateT e m) where
  failure message = ResolverState $ do
    selection <- asks currentSelection
    lift $ failure [resolverFailureMessage selection message]

instance (Monad m) => Failure InternalError (ResolverStateT e m) where
  failure message = ResolverState $ do
    selection <- asks currentSelection
    lift $ failure [renderInternalResolverError selection message]

instance (Monad m) => Failure GQLErrors (ResolverStateT e m) where
  failure = ResolverState . lift . failure

instance (Monad m) => PushEvents e (ResolverStateT e m) where
  pushEvents = ResolverState . lift . pushEvents

mapResolverState ::
  ( ResultT e m a ->
    ResultT e' m' a'
  ) ->
  ResolverStateT e m a ->
  ResolverStateT e' m' a'
mapResolverState f (ResolverState x) = ResolverState (mapReaderT f x)

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

renderInternalResolverError :: Selection VALID -> InternalError -> GQLError
renderInternalResolverError Selection {selectionName, selectionPosition} message =
  GQLError
    { message = msg message,
      locations = [selectionPosition]
    }
