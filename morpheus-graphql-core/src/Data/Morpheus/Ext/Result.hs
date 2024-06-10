{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.Result
  ( Result (..),
    ResultT (..),
    mapEvent,
    cleanEvents,
    PushEvents (..),
    resultOr,
    sortErrors,
    toEither,
    GQLResult,
  )
where

import Control.Monad.Except (MonadError (..))
import qualified Data.List.NonEmpty as NE
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError (..),
  )
import Data.Text.Lazy.Builder ()
import Relude

type GQLResult = Result GQLError

-- EVENTS
class PushEvents e m where
  pushEvents :: [e] -> m ()

--
-- Result
--
--
data Result err a
  = Success {result :: a, warnings :: [err]}
  | Failure {errors :: NonEmpty err}
  deriving (Functor)

instance Applicative (Result er) where
  pure x = Success x []
  Success f w1 <*> Success x w2 = Success (f x) (w1 <> w2)
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure (e :| es) <*> Success _ w = Failure (e :| es <> w)
  Success _ w <*> Failure (e :| es) = Failure (e :| es <> w)

instance Monad (Result er) where
  return = pure
  Success v w1 >>= fm = case fm v of
    (Success x w2) -> Success x (w1 <> w2)
    (Failure (e :| es)) -> Failure (e :| es <> w1)
  Failure e >>= _ = Failure e

instance Bifunctor Result where
  bimap f g Success {..} = Success {warnings = f <$> warnings, result = g result, ..}
  bimap f _ Failure {..} = Failure (f <$> errors)

instance MonadError er (Result er) where
  throwError = Failure . pure
  catchError (Failure (x :| _)) f = f x
  catchError x _ = x

instance (IsString err) => MonadFail (Result err) where
  fail = Failure . pure . fromString

resultOr :: (NonEmpty err -> a') -> (a -> a') -> Result err a -> a'
resultOr _ f Success {result} = f result
resultOr f _ Failure {errors} = f errors

sortErrors :: Result GQLError a -> Result GQLError a
sortErrors (Failure errors) = Failure (NE.sort errors)
sortErrors x = x

-- ResultT
newtype ResultT event (m :: Type -> Type) a = ResultT
  { runResultT :: m (Result GQLError ([event], a))
  }
  deriving (Functor)

instance (Applicative m) => Applicative (ResultT event m) where
  pure = ResultT . pure . pure . ([],)
  ResultT app1 <*> ResultT app2 = ResultT $ liftA2 (<*>) (fx <$> app1) app2
    where
      fx :: (Monad f) => f ([event], a -> b) -> f (([event], a) -> ([event], b))
      fx x = do
        (e', f) <- x
        pure $ \(e, a) -> (e <> e', f a)

instance (Monad m) => Monad (ResultT event m) where
  return = pure
  (ResultT m1) >>= mFunc = ResultT $ do
    result <- m1
    case result of
      Failure errors -> pure $ Failure errors
      Success (events, value) w1 -> do
        result' <- runResultT (mFunc value)
        case result' of
          Failure (e :| es) -> pure $ Failure (e :| es <> w1)
          Success (events', value') w2 -> pure $ Success (events <> events', value') (w1 <> w2)

instance MonadTrans (ResultT event) where
  lift = ResultT . fmap (pure . ([],))

instance (Monad m) => MonadError GQLError (ResultT event m) where
  throwError = ResultT . pure . throwError
  catchError (ResultT mx) f = ResultT (mx >>= catchResultError)
    where
      catchResultError (Failure (x :| _)) = runResultT (f x)
      catchResultError x = pure x

instance (Applicative m) => PushEvents event (ResultT event m) where
  pushEvents x = ResultT $ pure $ pure (x, ())

cleanEvents ::
  (Functor m) =>
  ResultT e m a ->
  ResultT e' m a
cleanEvents resT = ResultT $ fmap (first (const [])) <$> runResultT resT

mapEvent ::
  (Monad m) =>
  (e -> e') ->
  ResultT e m value ->
  ResultT e' m value
mapEvent func (ResultT ma) = ResultT $ fmap (first (map func)) <$> ma

toEither :: Result err b -> Either (NonEmpty err) b
toEither = resultOr Left Right
