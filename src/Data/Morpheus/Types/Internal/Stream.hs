{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Morpheus.Types.Internal.Stream
  ( StreamState(..)
  , StreamT(..)
  , ResponseEvent(..)
  , ResponseStream
  , EventContent
  , SubPair
  , PubPair
  , closeStream
  , mapEvent
  ) where

import           Data.Morpheus.Types.IO (GQLResponse)

data family EventContent conf :: *

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

type Pair channel content = ([channel], content)

type SubPair m s = Pair s (EventContent s -> m GQLResponse)

type PubPair s = Pair s (EventContent s)

type ResponseStream m event a = StreamT m (ResponseEvent m event) a

data ResponseEvent m s
  = Publish (PubPair s)
  | Subscribe (SubPair m s)

toTuple :: StreamState s a -> ([s], a)
toTuple StreamState {streamEvents, streamValue} = (streamEvents, streamValue)

closeStream :: Monad m => (StreamT m s) v -> m ([s], v)
closeStream resolver = toTuple <$> runStreamT resolver

mapEvent :: Monad m => (a -> b) -> StreamT m a value -> StreamT m b value
mapEvent func (StreamT ma) =
  StreamT $ do
    state <- ma
    return $ state {streamEvents = map func (streamEvents state)}
