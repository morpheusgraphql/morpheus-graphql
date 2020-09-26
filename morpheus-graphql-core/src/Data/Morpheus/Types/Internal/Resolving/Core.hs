{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving.Core
  ( Eventless,
    Result (..),
    Failure (..),
    ResultT (..),
    unpackEvents,
    mapEvent,
    cleanEvents,
    PushEvents (..),
    resultOr,
    sortErrors,
  )
where

import Control.Applicative (Applicative (..), liftA2)
import Control.Monad (Monad (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor ((<$>), Functor (..))
import Data.List (sortOn)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLError (..),
    GQLErrors,
    ValidationError (..),
    toGQLError,
  )
import Data.Semigroup ((<>))
import Prelude
  ( ($),
    (.),
  )

type Eventless = Result ()

-- EVENTS
class PushEvents e m where
  pushEvents :: [e] -> m ()

unpackEvents :: Result event a -> [event]
unpackEvents Success {events} = events
unpackEvents _ = []

--
-- Result
--
--
data Result events a
  = Success {result :: a, warnings :: GQLErrors, events :: [events]}
  | Failure {errors :: GQLErrors}
  deriving (Functor)

instance Applicative (Result e) where
  pure x = Success x [] []
  Success f w1 e1 <*> Success x w2 e2 = Success (f x) (w1 <> w2) (e1 <> e2)
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e <*> Success _ w _ = Failure (e <> w)
  Success _ w _ <*> Failure e = Failure (e <> w)

instance Monad (Result e) where
  return = pure
  Success v w1 e1 >>= fm = case fm v of
    (Success x w2 e2) -> Success x (w1 <> w2) (e1 <> e2)
    (Failure e) -> Failure (e <> w1)
  Failure e >>= _ = Failure e

instance Failure [GQLError] (Result ev) where
  failure = Failure

instance PushEvents events (Result events) where
  pushEvents events = Success {result = (), warnings = [], events}

instance Failure [ValidationError] (Result ev) where
  failure = failure . fmap toGQLError

resultOr :: (GQLErrors -> a') -> (a -> a') -> Result e a -> a'
resultOr _ f (Success x _ _) = f x
resultOr f _ (Failure e) = f e

sortErrors :: Result e a -> Result e a
sortErrors (Failure errors) = Failure (sortOn locations errors)
sortErrors x = x

-- ResultT
newtype ResultT event (m :: * -> *) a = ResultT
  { runResultT :: m (Result event a)
  }
  deriving (Functor)

instance Applicative m => Applicative (ResultT event m) where
  pure = ResultT . pure . pure
  ResultT app1 <*> ResultT app2 = ResultT $ liftA2 (<*>) app1 app2

instance Monad m => Monad (ResultT event m) where
  return = pure
  (ResultT m1) >>= mFunc = ResultT $ do
    result1 <- m1
    case result1 of
      Failure errors -> pure $ Failure errors
      Success value1 w1 e1 -> do
        result2 <- runResultT (mFunc value1)
        case result2 of
          Failure errors -> pure $ Failure (errors <> w1)
          Success v2 w2 e2 -> pure $ Success v2 (w1 <> w2) (e1 <> e2)

instance MonadTrans (ResultT event) where
  lift = ResultT . fmap pure

instance Monad m => Failure GQLErrors (ResultT event m) where
  failure = ResultT . pure . failure

instance Applicative m => PushEvents event (ResultT event m) where
  pushEvents = ResultT . pure . pushEvents

cleanEvents ::
  Functor m =>
  ResultT e m a ->
  ResultT e' m a
cleanEvents resT = ResultT $ replace <$> runResultT resT
  where
    replace (Success v w _) = Success v w []
    replace (Failure e) = Failure e

mapEvent ::
  Monad m =>
  (e -> e') ->
  ResultT e m value ->
  ResultT e' m value
mapEvent func (ResultT ma) = ResultT $ mapEv <$> ma
  where
    mapEv Success {result, warnings, events} =
      Success {result, warnings, events = fmap func events}
    mapEv (Failure err) = Failure err
