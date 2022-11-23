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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Resolver
  ( Resolver,
    LiftOperation,
    lift,
    subscribe,
    ResponseEvent (..),
    ResponseStream,
    WithOperation,
    ResolverContext (..),
    withArguments,
    SubscriptionField (..),
    runResolver,
    getArgument,
    MonadResolver (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Reader (mapReaderT)
import Data.Morpheus.App.Internal.Resolving.Event
  ( EventHandler (..),
    ResponseEvent (..),
  )
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverState,
    ResolverStateT (..),
    clearStateResolverEvents,
    resolverFailureMessage,
    runResolverState,
    runResolverStateT,
    runResolverStateValueM,
    toResolverStateT,
  )
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    PushEvents (..),
    Result (..),
    ResultT (..),
    cleanEvents,
    mapEvent,
  )
import Data.Morpheus.Internal.Utils (selectOr)
import Data.Morpheus.Types.IO
  ( GQLResponse,
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (argumentValue),
    Arguments,
    FieldName,
    GQLError,
    MUTATION,
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    Selection (..),
    VALID,
    ValidValue,
    Value (..),
    msg,
  )
import Relude hiding
  ( Show,
    empty,
    show,
  )
import Prelude (Show (..))

type WithOperation (o :: OperationType) = LiftOperation o

type ResponseStream event (m :: Type -> Type) = ResultT (ResponseEvent event m) m

data SubscriptionField (a :: Type) where
  SubscriptionField ::
    { channel :: forall e m v. a ~ Resolver SUBSCRIPTION e m v => Channel e,
      unSubscribe :: a
    } ->
    SubscriptionField a

class Monad m => MonadResolver (m :: Type -> Type) where
  type MonadOperation m :: OperationType
  type MonadEvent m :: Type
  liftState :: ResolverState a -> m a
  getArguments :: m (Arguments VALID)

instance (LiftOperation o, Monad m) => MonadResolver (Resolver o e m) where
  type MonadOperation (Resolver o e m) = o
  type MonadEvent (Resolver o e m) = e
  getArguments = asks (selectionArguments . currentSelection)
  liftState = packResolver . toResolverStateT

--liftState
-- GraphQL Field Resolver
--
---------------------------------------------------------------
data Resolver (o :: OperationType) event (m :: Type -> Type) value where
  ResolverQ :: {runResolverQ :: ResolverStateT () m value} -> Resolver QUERY event m value
  ResolverM :: {runResolverM :: ResolverStateT event m value} -> Resolver MUTATION event m value
  ResolverS :: {runResolverS :: ResolverStateT () m (SubEventRes event m value)} -> Resolver SUBSCRIPTION event m value

type SubEventRes event m value = ReaderT event (ResolverStateT () m) value

instance Show (Resolver o e m value) where
  show ResolverQ {} = "Resolver QUERY e m a"
  show ResolverM {} = "Resolver MUTATION e m a"
  show ResolverS {} = "Resolver SUBSCRIPTION e m a"

deriving instance (Functor m) => Functor (Resolver o e m)

-- Applicative
instance (LiftOperation o, Monad m) => Applicative (Resolver o e m) where
  pure = packResolver . pure
  ResolverQ r1 <*> ResolverQ r2 = ResolverQ $ r1 <*> r2
  ResolverM r1 <*> ResolverM r2 = ResolverM $ r1 <*> r2
  ResolverS r1 <*> ResolverS r2 = ResolverS $ (<*>) <$> r1 <*> r2

-- Monad
instance (Monad m, LiftOperation o) => Monad (Resolver o e m) where
  return = pure
  (ResolverQ x) >>= m2 = ResolverQ (x >>= runResolverQ . m2)
  (ResolverM x) >>= m2 = ResolverM (x >>= runResolverM . m2)
  (ResolverS res) >>= m2 = ResolverS (liftSubResolver m2 <$> res)

liftSubResolver ::
  (Monad m) =>
  (t -> Resolver SUBSCRIPTION r m a) ->
  ReaderT r (ResolverStateT () m) t ->
  ReaderT r (ResolverStateT () m) a
liftSubResolver m2 readResA = ReaderT $ \e -> do
  a <- runReaderT readResA e
  readResB <- runResolverS (m2 a)
  runReaderT readResB e

-- MonadIO
instance (MonadIO m, LiftOperation o) => MonadIO (Resolver o e m) where
  liftIO = lift . liftIO

-- Monad Transformers
instance (LiftOperation o) => MonadTrans (Resolver o e) where
  lift = packResolver . lift

-- Failure
instance (LiftOperation o, Monad m) => MonadError GQLError (Resolver o e m) where
  throwError = packResolver . throwError
  catchError (ResolverQ r) f = ResolverQ $ catchError r (runResolverQ . f)
  catchError (ResolverM r) f = ResolverM $ catchError r (runResolverM . f)
  catchError (ResolverS r) f = ResolverS $ catchError r (runResolverS . f)

instance (Monad m, LiftOperation o) => MonadFail (Resolver o e m) where
  fail = throwError . msg

-- PushEvents
instance (Monad m) => PushEvents e (Resolver MUTATION e m) where
  pushEvents = packResolver . pushEvents

instance (Monad m, Semigroup a, LiftOperation o) => Semigroup (Resolver o e m a) where
  x <> y = fmap (<>) x <*> y

instance (LiftOperation o, Monad m) => MonadReader ResolverContext (Resolver o e m) where
  ask = packResolver ask
  local f (ResolverQ res) = ResolverQ (local f res)
  local f (ResolverM res) = ResolverM (local f res)
  local f (ResolverS resM) = ResolverS $ mapReaderT (local f) <$> resM

class LiftOperation (o :: OperationType) where
  packResolver :: Monad m => ResolverStateT e m a -> Resolver o e m a

instance LiftOperation QUERY where
  packResolver = ResolverQ . clearStateResolverEvents

instance LiftOperation MUTATION where
  packResolver = ResolverM

instance LiftOperation SUBSCRIPTION where
  packResolver = ResolverS . pure . lift . clearStateResolverEvents

subscribe ::
  (Monad m) =>
  Channel e ->
  Resolver QUERY e m (e -> Resolver SUBSCRIPTION e m a) ->
  SubscriptionField (Resolver SUBSCRIPTION e m a)
subscribe ch res =
  SubscriptionField ch $
    ResolverS $
      fromSub <$> runResolverQ res
  where
    fromSub :: Monad m => (e -> Resolver SUBSCRIPTION e m a) -> ReaderT e (ResolverStateT () m) a
    fromSub f = join (ReaderT (runResolverS . f))

withArguments :: (MonadResolver m) => (Arguments VALID -> m a) -> m a
withArguments = (getArguments >>=)

getArgument :: (MonadResolver m) => FieldName -> m (Value VALID)
getArgument name = selectOr Null argumentValue name <$> getArguments

runResolver ::
  Monad m =>
  Maybe (Selection VALID -> ResolverState (Channel event)) ->
  Resolver o event m ValidValue ->
  ResolverContext ->
  ResponseStream event m ValidValue
runResolver _ (ResolverQ resT) sel = cleanEvents $ runResolverStateT resT sel
runResolver _ (ResolverM resT) sel = mapEvent Publish $ runResolverStateT resT sel
runResolver toChannel (ResolverS resT) ctx = ResultT $ do
  readResValue <- runResolverStateValueM resT ctx
  pure $ case readResValue >>= subscriptionEvents ctx toChannel . toEventResolver ctx of
    Failure x -> Failure x
    Success {warnings, result} ->
      Success
        { warnings,
          result = ([result], Null)
        }

toEventResolver :: Monad m => ResolverContext -> SubEventRes event m ValidValue -> (event -> m GQLResponse)
toEventResolver sel (ReaderT subRes) event = renderResponse <$> runResolverStateValueM (subRes event) sel

subscriptionEvents ::
  ResolverContext ->
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  (e -> m GQLResponse) ->
  GQLResult (ResponseEvent e m)
subscriptionEvents ctx@ResolverContext {currentSelection} (Just channelGenerator) res =
  runResolverState handle ctx
  where
    handle = do
      channel <- channelGenerator currentSelection
      pure $ Subscribe channel res
subscriptionEvents ctx Nothing _ = throwError $ resolverFailureMessage ctx "channel Resolver is not defined"
