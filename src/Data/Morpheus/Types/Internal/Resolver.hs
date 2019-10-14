{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Types.Internal.Resolver
  ( Pure
  , Resolver
  , MutResolver
  , SubResolver
  , ResolveT
  , SubResolveT
  , SubRootRes
  , Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
  , resolver
  , GQLFail(..)
  , ResponseT
  , failResolveT
  , GADTResolver(..)
  , extractMutResolver
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..))
import           Data.Text                               (pack, unpack)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Base       (Message)
import           Data.Morpheus.Types.Internal.Data       (OperationKind (..))
import           Data.Morpheus.Types.Internal.Stream     (Event (..), PublishStream, ResponseStream, StreamChannel,
                                                          StreamState (..), StreamT (..), SubscribeStream)
import           Data.Morpheus.Types.Internal.Validation (GQLErrors)

class Monad m =>
      GQLFail (t :: (* -> *) -> * -> *) m
  where
  gqlFail :: Monad m => Message -> t m a
  toSuccess :: Monad m => (Message -> b) -> (a -> b) -> t m a -> t m b

instance Monad m => GQLFail Resolver m where
  gqlFail = ExceptT . pure . Left . unpack
  toSuccess fFail fSuc (ExceptT value) = ExceptT $ pure . mapCases <$> value
    where
      mapCases (Right x) = fSuc x
      mapCases (Left x)  = fFail $ pack $ show x

----------------------------------------------------------------------------------------
type SubResolver = GADTResolver 'Subscription

type MutResolver = GADTResolver 'Mutation


data GADTResolver (o::OperationKind) (m :: * -> * ) event value where
    QueryResolver:: m value -> GADTResolver 'Query m  event value
    MutationResolver :: [event] -> m value -> GADTResolver 'Mutation m event value
    SubscriptionResolver :: [StreamChannel event] -> (event -> Resolver m value) -> GADTResolver 'Subscription m event value
    FailedResolving :: String -> GADTResolver o m event value

type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (SubResolver m e) = Resolver m

-------------------------------------------------------------------
type Resolver = ExceptT String


-- TODO: Replace With: newtype MutResolver
-- newtype MutResolver m e a = MutResolver {  unMutResolveT :: Resolver (PublishStream m e) a }

type SubRootRes m e sub = Resolver (SubscribeStream m e) sub

--- Transformers
type ResponseT m e  = ResolveT (ResponseStream m e )

type SubResolveT m e a = ResolveT (SubscribeStream m e) (e -> ResolveT m a)

type ResolveT = ExceptT GQLErrors

failResolveT :: Monad m => GQLErrors -> ResolveT m a
failResolveT = ExceptT . pure . Left

-- TODO: use it
--data GraphQLT (o::OperationKind) (m :: * -> * ) event value where
--    QueryT:: ExceptT GQLErrors m value -> GraphQLT 'Query m  () value
--    MutationT :: ExceptT GQLErrors (PublishStream m event) value -> GraphQLT 'Mutation m event value
--    SubscriptionT ::  ExceptT GQLErrors (SubscribeStream m event) (event -> ExceptT GQLErrors m value) -> GraphQLT 'Subscription m event value

-------------------------------------------------------------------
-- | Pure Resolver without effect
type Pure = Either String

-- | GraphQL Resolver
resolver :: m (Either String a) -> Resolver m a
resolver = ExceptT

extractMutResolver :: Monad m => MutResolver m e a -> Resolver (PublishStream m e) a
extractMutResolver (MutationResolver channels res) = (ExceptT . StreamT . fmap (StreamState channels . Right) )  res

-- | GraphQL Resolver for mutation or subscription resolver , adds effect to normal resolver
-- mutResolver :: Monad m => [e] -> (StreamT m e) (Either String a) -> MutResolver m e a
-- mutResolver channels = ExceptT . StreamT . fmap effectPlus . runStreamT
--  where
--    effectPlus state = state {streamEvents = channels ++ streamEvents state}

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: Resolver m (query (Resolver m))
  , mutationResolver     :: Resolver (PublishStream m event) (mut (MutResolver m event))
  , subscriptionResolver :: SubRootRes m event (sub (SubResolver  m event))
  }
