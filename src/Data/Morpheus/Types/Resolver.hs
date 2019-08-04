{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Types.Resolver
  ( Pure
  , Resolver
  , MutResolver
  , SubResolver
  , ResolveT
  , SubResolveT
  , MutResolveT
  , SubRootRes
  , Event(..)
  , GQLRootResolver(..)
  , resolver
  , mutResolver
  , toMutResolver
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..), runExceptT)

-- MORPHEUS
--
import           Data.Morpheus.Types.Internal.Stream     (Event (..), PublishStream, StreamState (..), StreamT (..),
                                                          SubscribeStream)
import           Data.Morpheus.Types.Internal.Validation (ResolveT)

-- SubResolver m (Event [c1, c2] d) a
----------------------------------------------------------------------------------------
type MutResolveT m e c a = ResolveT (PublishStream m e c) a

-------------------------------------------------------------------
type Resolver = ExceptT String

type MutResolver m e c = Resolver (PublishStream m e c)

type SubResolver m e c a = Event e (c -> Resolver m a)

type SubResolveT m e c a = ResolveT (SubscribeStream m e) (c -> ResolveT m a)

type SubRootRes m e sub = Resolver (SubscribeStream m e) sub

-------------------------------------------------------------------
-- | Pure Resolver without effect
type Pure = Either String

-- | GraphQL Resolver
resolver :: m (Either String a) -> Resolver m a
resolver = ExceptT

toMutResolver :: Monad m => [c] -> Resolver m a -> Resolver (StreamT m c) a
toMutResolver channels = ExceptT . StreamT . fmap (StreamState channels) . runExceptT

-- | GraphQL Resolver for mutation or subscription resolver , adds effect to normal resolver
mutResolver :: Monad m => [c] -> (StreamT m c) (Either String a) -> Resolver (StreamT m c) a
mutResolver channels = ExceptT . StreamT . fmap effectPlus . runStreamT
  where
    effectPlus state = state {streamEvents = channels ++ streamEvents state}

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver m e c query mut sub = GQLRootResolver
  { queryResolver        :: Resolver m query
  , mutationResolver     :: Resolver (PublishStream m e c) mut
  , subscriptionResolver :: SubRootRes m e sub
  }
