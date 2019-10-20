{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Types.Internal.Stream
  ( StreamState(..)
  , ResponseEvent(..)
  , SubEvent
  , Event(..)
  -- STREAMS
  , StreamT(..)
  , ResponseStream
  , closeStream
  , mapS
  , injectEvents
  , initExceptStream
  , pushEvents
 -- , GQLMonad(..)
  , GQLChannel(..)
  , Channel(..)
  ) where

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Data.Semigroup ((<>))

-- MORPHEUS
import           Data.Morpheus.Types.IO     (GQLResponse)

-- EVENTS
data ResponseEvent m event
  = Publish event
  | Subscribe (SubEvent m event)

-- STREAMS
type ResponseStream m event = StreamT m (ResponseEvent m event)

type SubEvent m event = Event (Channel event) (event-> m GQLResponse)

newtype Channel event = Channel {
  unChannel :: StreamChannel event
}

instance (Eq (StreamChannel event)) => Eq (Channel event) where
  Channel x == Channel y = x == y


class GQLChannel a where
    type StreamChannel a :: *
    streamChannels :: a -> [Channel a]

instance GQLChannel () where
    type StreamChannel () = ()
    streamChannels _ = []

instance GQLChannel (Event channel content)  where
   type StreamChannel (Event channel content)  = channel
   streamChannels Event { channels } =  map Channel channels

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

pushEvents :: Functor m => [event] -> ExceptT e (StreamT m event) a -> ExceptT e (StreamT m event) a
pushEvents events = ExceptT . StreamT . fmap updateState . runStreamT . runExceptT
    where
        updateState x = x { streamEvents = events <> streamEvents x }

injectEvents :: Functor m => [event] -> ExceptT e m a -> ExceptT e (StreamT m event) a
injectEvents states = ExceptT . StreamT . fmap (StreamState states) . runExceptT

initExceptStream :: Applicative m => [event] -> a -> ExceptT e (StreamT m event) a
initExceptStream events = ExceptT . StreamT . pure . StreamState events . Right
