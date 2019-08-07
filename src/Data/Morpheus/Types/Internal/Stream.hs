{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

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
  ) where

import           Data.Morpheus.Types.IO (GQLResponse)

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

type ResponseStream m event con a = StreamT m (ResponseEvent m event con) a

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
