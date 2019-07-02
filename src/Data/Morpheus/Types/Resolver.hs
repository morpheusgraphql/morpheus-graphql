{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Types.Resolver
  ( Pure
  , ResM
  , StreamM
  , StreamT(..)
  , Stream(..)
  , Resolver
  , GQLRootResolver(..)
  , gqlResolver
  , gqlStreamResolver
  , liftStreamResolver
  , unpackStream
  , unpackStream2
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..), runExceptT)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Validation (GQLErrors, ResolveT)
import           Data.Morpheus.Types.Internal.Value      (Value)

-- | Pure Resolver without effect
type Pure = Either String

-- | Monad IO resolver without GraphQL effect
type ResM = Resolver IO

-- | Monad Resolver with GraphQL effects, used for communication between mutation and subscription
type StreamM c = Resolver (StreamT IO c)

-- | Resolver Monad Transformer
type Resolver = ExceptT String

-- | GraphQL Resolver
gqlResolver :: m (Either String a) -> Resolver m a
gqlResolver = ExceptT

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver m s a b c = GQLRootResolver
  { queryResolver        :: ResolveT m a
  , mutationResolver     :: ResolveT (StreamT m s) b
  , subscriptionResolver :: ResolveT (StreamT m (s, s -> ResolveT m Value)) c
  }

-- | GraphQL Resolver for mutation or subscription resolver , adds effect to normal resolver
gqlStreamResolver :: Monad m => [c] -> (StreamT m c) (Either String a) -> Resolver (StreamT m c) a
gqlStreamResolver channels = ExceptT . insertStream channels

insertStream :: Monad m => [c] -> StreamT m c a -> StreamT m c a
insertStream channels StreamT {runStreamT = monadStream} = StreamT $ effectPlus <$> monadStream
  where
    effectPlus x = x {resultStreams = channels ++ resultStreams x}

-- | lift Normal resolver inside Stream Resolver
liftStreamResolver :: Monad m => [c] -> m (Either String a) -> Resolver (StreamT m c) a
liftStreamResolver channels = ExceptT . StreamT . fmap (Stream channels)

unpackStream2 :: Monad m => ResolveT (StreamT m s) v -> ResolveT m ([s], v)
unpackStream2 x = ExceptT $ unpackStream x

unpackStream :: Monad m => ResolveT (StreamT m s) v -> m (Either GQLErrors ([s], v))
unpackStream resolver = do
  (Stream effects eitherValue) <- runStreamT $ runExceptT resolver
  case eitherValue of
    Left errors -> return $ Left errors
    Right value -> return $ Right (effects, value)

data Stream c v = Stream
  { resultStreams :: [c]
  , resultValue   :: v
  } deriving (Functor)

-- | Monad Transformer that sums all effect Together
newtype StreamT m c v = StreamT
  { runStreamT :: m (Stream c v)
  } deriving (Functor)

instance Monad m => Applicative (StreamT m c) where
  pure = StreamT . return . Stream []
  StreamT app1 <*> StreamT app2 =
    StreamT $ do
      (Stream effect1 func) <- app1
      (Stream effect2 val) <- app2
      return $ Stream (effect1 ++ effect2) (func val)

instance Monad m => Monad (StreamT m c) where
  return = pure
  (StreamT m1) >>= mFunc =
    StreamT $ do
      (Stream e1 v1) <- m1
      (Stream e2 v2) <- runStreamT $ mFunc v1
      return $ Stream (e1 ++ e2) v2
