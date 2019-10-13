{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies , GADTs     #-}

module Data.Morpheus.Types.Internal.Stream
  ( StreamState(..)
  , ResponseEvent(..)
  , SubEvent
  , PubEvent
  , Event(..)
  -- STREAMS
  , StreamT(..)
  , SubscribeStream
  , PublishStream
  , ResponseStream
  , closeStream
  , mapS
  , injectEvents
  , initExceptStream
  , GQLMonad(..)
  ) where

import           Control.Monad.Trans.Except        (ExceptT (..), runExceptT)
import           Data.Morpheus.Types.Internal.Data (OperationKind (..))
import           Data.Morpheus.Types.IO            (GQLResponse)

data GQLMonad (o::OperationKind) (m :: * -> * ) value where 
    QueryM :: m value -> GQLMonad 'Query m  ()
    MutationM :: [Event channel event] -> m value -> GQLMonad 'Mutation m (Event channel event)
    SubscriptionM ::  [channel] -> m (Event channel event -> m value) -> GQLMonad 'Subscription m (Event channel event)

data Event e c = Event
  { channels :: [e]
  , content  :: c
  }

data StreamState c v = StreamState
  { streamEvents :: [c]
  , streamValue  :: v
  } deriving (Functor)

-- | Monad Transformer that sums all effect Together
newtype StreamT m s a = StreamT
  { runStreamT :: m (StreamState s a)
  } deriving (Functor)

instance Monad m => Applicative (StreamT m c) where
  pure = StreamT . return . StreamState []
  StreamT app1 <*> StreamT app2 =
    StreamT $ do
      (StreamState effect1 func) <- app1
      (StreamState effect2 val) <- app2
      return $ StreamState (effect1 ++ effect2) (func val)

instance Monad m => Monad (StreamT m c) where
  return = pure
  (StreamT m1) >>= mFunc =
    StreamT $ do
      (StreamState e1 v1) <- m1
      (StreamState e2 v2) <- runStreamT $ mFunc v1
      return $ StreamState (e1 ++ e2) v2

type SubEvent m e c = Event e (Event e c -> m GQLResponse)

type PubEvent e c = Event e c

-- EVENTS
data ResponseEvent m e c
  = Publish (PubEvent e c)
  | Subscribe (SubEvent m e c)

-- STREAMS
type SubscribeStream m e = StreamT m [e]

type PublishStream m e c = StreamT m (PubEvent e c)

type ResponseStream m event con = StreamT m (ResponseEvent m event con)

-- Helper Functions
toTuple :: StreamState s a -> ([s], a)
toTuple StreamState {streamEvents, streamValue} = (streamEvents, streamValue)

closeStream :: Monad m => (StreamT m s) v -> m ([s], v)
closeStream resolver = toTuple <$> runStreamT resolver

mapS :: Monad m => (a -> b) -> StreamT m a value -> StreamT m b value
mapS func (StreamT ma) =
  StreamT $ do
    state <- ma
    return $ state {streamEvents = map func (streamEvents state)}

injectEvents :: Functor m => [event] -> ExceptT e m a -> ExceptT e (StreamT m event) a
injectEvents states = ExceptT . StreamT . fmap (StreamState states) . runExceptT

initExceptStream :: Applicative m => [event] -> a -> ExceptT e (StreamT m event) a
initExceptStream events = ExceptT . StreamT . pure . StreamState events . Right
