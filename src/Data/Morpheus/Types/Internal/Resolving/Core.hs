{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.Morpheus.Types.Internal.Resolving.Core
  ( Validation
  , Result(..)
  , Failure(..)
  , ResultT(..)
  , unpackEvents
  , LibUpdater
  , resolveUpdates
  , mapEvent
  , cleanEvents
  , Event(..)
  , Channel(..)
  , GQLChannel(..)
  , PushEvents(..)
  , mapError
  )
where

import           Control.Monad                  ( foldM )
import           Data.Function                  ( (&) )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Applicative            ( liftA2 )
import           Data.Morpheus.Types.Internal.Operation
                                                ( 
                                                  Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Message 
                                                , GQLErrors
                                                , GQLError(..)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Semigroup                 ( (<>) )


type Validation = Result () GQLError

mapError 
  :: (er1 -> er2)
  -> Result ev er1 a
  -> Result ev er2 a
mapError f Success { warnings = ws , .. } =
    Success { warnings = fmap f ws , .. }
mapError f (Failure es) = Failure $ fmap f es

-- EVENTS
class PushEvents e m where 
  pushEvents :: [e] -> m () 

-- Channel
newtype Channel event = Channel {
  _unChannel :: StreamChannel event
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
  { channels :: [e], content  :: c}


unpackEvents :: Result event error a -> [event]
unpackEvents Success { events } = events
unpackEvents _                  = []

--
-- Result
--
--
data Result events error a 
  = Success { result :: a , warnings :: [error] , events:: [events] }
  | Failure { errors :: [error] } deriving (Functor)

instance Applicative (Result e error) where
  pure x = Success x [] []
  Success f w1 e1 <*> Success x w2 e2 = Success (f x) (w1 <> w2) (e1 <> e2)
  Failure e1      <*> Failure e2      = Failure (e1 <> e2)
  Failure e       <*> Success _ w _   = Failure (e <> w)
  Success _ w _   <*> Failure e       = Failure (e <> w)

instance Monad (Result e error)  where
  return = pure
  Success v w1 e1 >>= fm = case fm v of
    (Success x w2 e2) -> Success x (w1 <> w2) (e1 <> e2)
    (Failure e      ) -> Failure (e <> w1)
  Failure e >>= _ = Failure e

instance Failure [error] (Result ev error) where
  failure = Failure

instance Failure Text Validation where
  failure text =
    Failure [GQLError { message = "INTERNAL ERROR: " <> text, locations = [] }]

instance PushEvents events (Result events err) where
  pushEvents events = Success { result = (), warnings = [], events } 

-- ResultT
newtype ResultT event error (m :: * -> * ) a 
  = ResultT 
    { 
      runResultT :: m (Result event error a)  
    }
    deriving (Functor)

instance Applicative m => Applicative (ResultT event error m) where
  pure = ResultT . pure . pure
  ResultT app1 <*> ResultT app2 = ResultT $ liftA2 (<*>) app1 app2

instance Monad m => Monad (ResultT event error m) where
  return = pure
  (ResultT m1) >>= mFunc = ResultT $ do
    result1 <- m1
    case result1 of
      Failure errors       -> pure $ Failure errors
      Success value1 w1 e1 -> do
        result2 <- runResultT (mFunc value1)
        case result2 of
          Failure errors   -> pure $ Failure (errors <> w1)
          Success v2 w2 e2 -> return $ Success v2 (w1 <> w2) (e1 <> e2)

instance MonadTrans (ResultT event error) where
  lift = ResultT . fmap pure

instance Monad m => Failure Message (ResultT event String m) where
  failure message = ResultT $ pure $ Failure [unpack message]

instance Applicative m => Failure String (ResultT ev GQLError m) where
  failure x =
    ResultT $ pure $ Failure [GQLError { message = pack x, locations = [] }]

instance Monad m => Failure GQLErrors (ResultT event GQLError m) where
  failure = ResultT . pure . failure

instance Applicative m => PushEvents events (ResultT events err m) where
  pushEvents = ResultT . pure . pushEvents

cleanEvents
  :: Functor m
  => ResultT e1 error m a
  -> ResultT e2 error m a
cleanEvents resT = ResultT $ replace <$> runResultT resT
 where
  replace (Success v w _) = Success v w []
  replace (Failure e    ) = Failure e


mapEvent
  :: Monad m
  => (ea -> eb)
  -> ResultT ea er m value
  -> ResultT eb er m value
mapEvent func (ResultT ma) = ResultT $ mapEv <$> ma
 where
  mapEv Success { result, warnings, events } =
    Success { result, warnings, events = map func events }
  mapEv (Failure err) = Failure err

-- mapError 
--  :: Monad m
--   => (er1 -> er2)
--   -> ResultT ev er1 con m value
--   -> ResultT ev er2 con m value
-- mapError f (ResultT ma) = ResultT $ mapEv <$> ma
--  where
--   mapEv Success { warnings = ws , .. } =
--     Success { warnings = fmap f ws , .. }
--   mapEv (Failure es) = Failure $ fmap f es

-- Helper Functions
type LibUpdater lib = lib -> Validation lib

resolveUpdates :: lib -> [LibUpdater lib] -> Validation lib
resolveUpdates = foldM (&)
