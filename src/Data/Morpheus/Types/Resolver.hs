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
  , SubResolver(..)
  , ResolveT
  , SubResolveT
  , MutResolveT
  , SubRootRes
  , Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
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

----------------------------------------------------------------------------------------
{--
newtype SubResolveT m e c a = SubResolveT
  { unSubResolveT :: ResolveT (SubscribeStream m e) (Event e c -> ResolveT m a)
  }

newtype MutResolveT m e c a = MutResolveT
  { unMutResolveT :: ResolveT (PublishStream m e c) a
  }
-}
data SubResolver m e c a = SubResolver
  { subChannels :: [e]
  , subResolver :: Event e c -> Resolver m a
  }

type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (SubResolver m e c) = Resolver m

-------------------------------------------------------------------
type Resolver = ExceptT String

type MutResolveT m e c a = ResolveT (PublishStream m e c) a

type SubResolveT m e c a = ResolveT (SubscribeStream m e) (Event e c -> ResolveT m a)

type MutResolver m e c = Resolver (PublishStream m e c)

type SubRootRes m e sub = Resolver (SubscribeStream m e) sub

-------------------------------------------------------------------
-- | Pure Resolver without effect
type Pure = Either String

-- | GraphQL Resolver
resolver :: m (Either String a) -> Resolver m a
resolver = ExceptT

toMutResolver :: Monad m => [Event e c] -> Resolver m a -> MutResolver m e c a
toMutResolver channels = ExceptT . StreamT . fmap (StreamState channels) . runExceptT

-- | GraphQL Resolver for mutation or subscription resolver , adds effect to normal resolver
mutResolver :: Monad m => [Event e c] -> (StreamT m (Event e c)) (Either String a) -> MutResolver m e c a
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
