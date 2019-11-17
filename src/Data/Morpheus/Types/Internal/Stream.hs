{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Types.Internal.Stream
  ( ResponseEvent(..)
  , SubEvent
  , Event(..)
  -- STREAMS
  , ResponseStream
  , closeStream
  , mapS
  , GQLChannel(..)
  , Channel(..)
  , mapFailure
  )
where

-- MORPHEUS
import           Data.Morpheus.Types.IO         ( GQLResponse )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLError
                                                , Result(..)
                                                , ResultT(..)
                                                )
-- EVENTS
data ResponseEvent m event
  = Publish event
  | Subscribe (SubEvent m event)

-- STREAMS
type ResponseStream event m = ResultT (ResponseEvent m event) GQLError 'True m

type SubEvent m event = Event (Channel event) (event -> m GQLResponse)

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
  type StreamChannel (Event channel content) = channel
  streamChannels Event { channels } = map Channel channels

data Event e c = Event
  { channels :: [e]
  , content  :: c
  }

-- Helper Functions
closeStream :: Monad m => ResultT e er con m value -> m (Result e con er value)
closeStream = runResultT

mapS
  :: Monad m
  => (ea -> eb)
  -> ResultT ea er con m value
  -> ResultT eb er con m value
mapS func (ResultT ma) = ResultT $ do
  state <- ma
  return $ state { events = map func (events state) }

mapFailure
  :: Monad m
  => (er1 -> er2)
  -> ResultT ev er1 con m value
  -> ResultT ev er2 con m value
mapFailure f (ResultT ma) = ResultT $ do
  state <- ma
  case state of
    Failure x     -> pure $ Failure (map f x)
    Success x w e -> pure $ Success x (map f w) e
