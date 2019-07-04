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
  , Resolver
  , GQLRootResolver(..)
  , gqlResolver
  , gqlStreamResolver
  , EventContent
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..))

-- MORPHEUS
--
import           Data.Morpheus.Types.Internal.Stream     (EventContent, PublishStream, StreamState (..), StreamT (..),
                                                          SubscribeStream)
import           Data.Morpheus.Types.Internal.Validation (ResolveT)

-- | Pure Resolver without effect
type Pure = Either String

-- | Monad IO resolver without GraphQL effect
type ResM = Resolver IO

type Resolver = ExceptT String

type SubRes m s b = ([s], EventContent s -> Resolver m b)

-- | Monad Resolver with GraphQL effects, used for communication between mutation and subscription
type StreamM s = Resolver (StreamT IO ([s], EventContent s))

-- | GraphQL Resolver
gqlResolver :: m (Either String a) -> Resolver m a
gqlResolver = ExceptT

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver m s query mut sub = GQLRootResolver
  { queryResolver        :: ResolveT m query
  , mutationResolver     :: ResolveT (PublishStream m s) mut
  , subscriptionResolver :: ResolveT (SubscribeStream m s) sub
  }

-- | GraphQL Resolver for mutation or subscription resolver , adds effect to normal resolver
gqlStreamResolver :: Monad m => [c] -> (StreamT m c) (Either String a) -> Resolver (StreamT m c) a
gqlStreamResolver channels = ExceptT . insertStream
  where
    insertStream (StreamT streamMonad) = StreamT $ effectPlus <$> streamMonad
    effectPlus x = x {streamEvents = channels ++ streamEvents x}
