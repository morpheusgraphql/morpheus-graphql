{-# LANGUAGE DataKinds             #-}
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
  , SubRes
  , SubStreamT
  , PubStreamT
  , StreamT(..)
  , Resolver
  , GQLRootResolver(..)
  , gqlResolver
  , gqlStreamResolver
  , liftStreamResolver
  , EventContent
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..))

-- MORPHEUS
--
import           Data.Morpheus.Types.Internal.Stream     (EventContent, StreamState (..), StreamT (..))
import           Data.Morpheus.Types.Internal.Validation (ResolveT)
import           Data.Morpheus.Types.Internal.Value      (Value)

-- | Pure Resolver without effect
type Pure = Either String

-- | Monad IO resolver without GraphQL effect
type ResM = Resolver IO

-- | Monad Resolver with GraphQL effects, used for communication between mutation and subscription
type StreamM s = Resolver (StreamT IO ([s], EventContent s))

-- | Resolver Monad Transformer
type Resolver = ExceptT String

-- | GraphQL Resolver
gqlResolver :: m (Either String a) -> Resolver m a
gqlResolver = ExceptT

type SubRes m s b = ([s], EventContent s -> Resolver m b)

type SubEvent m s = ([s], EventContent s -> ResolveT m Value)

type PubEvent s = ([s], EventContent s)

type PubStreamT m s = StreamT m (PubEvent s)

type SubStreamT m s = StreamT m (SubEvent m s)

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver m s query mut sub = GQLRootResolver
  { queryResolver        :: ResolveT m query
  , mutationResolver     :: ResolveT (PubStreamT m s) mut
  , subscriptionResolver :: ResolveT (SubStreamT m s) sub
  }

-- | GraphQL Resolver for mutation or subscription resolver , adds effect to normal resolver
gqlStreamResolver :: Monad m => [c] -> (StreamT m c) (Either String a) -> Resolver (StreamT m c) a
gqlStreamResolver channels = ExceptT . insertStream
  where
    insertStream (StreamT streamMonad) = StreamT $ effectPlus <$> streamMonad
    effectPlus x = x {streamEvents = channels ++ streamEvents x}

-- | lift Normal resolver inside Stream Resolver
liftStreamResolver :: Monad m => [c] -> m (Either String a) -> Resolver (StreamT m c) a
liftStreamResolver channels = ExceptT . StreamT . fmap (StreamState channels)
