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
  , Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
  , resolver
  , GQLFail(..)
  , ResponseT
  , failResolveT
  , GADTResolver(..)
  , GraphQLT(..)
  , PackT(..)
  , liftResolver
  , convertResolver
  , toResponseRes
  ) where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Text                                  (pack, unpack)

-- MORPHEUS
import           Data.Morpheus.Error.Selection              (resolverError)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..))
import           Data.Morpheus.Types.Internal.Base          (Message, Position)
import           Data.Morpheus.Types.Internal.Data          (Key, MUTATION, OperationKind, QUERY, SUBSCRIPTION)
import           Data.Morpheus.Types.Internal.Stream        (Event (..), PublishStream, ResponseEvent (..),
                                                             ResponseStream, StreamChannel, StreamState (..),
                                                             StreamT (..), SubscribeStream, closeStream, mapS)
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Types.Internal.Value         (GQLValue (..), Value)
import           Data.Morpheus.Types.IO                     (renderResponse)

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
type SubResolver = GADTResolver SUBSCRIPTION

type MutResolver = GADTResolver MUTATION

-- type QueResolver = GADTResolver 'Query

-- TODO: Replace With: newtype MutResolver
-- newtype MutResolver m e a = MutResolver {  unMutResolveT :: Resolver (PublishStream m e) a }

type Resolver = ExceptT String

type ResolveT = ExceptT GQLErrors

------------------------------------------------------------
--- Transformers
type ResponseT m e  = ResolveT (ResponseStream m e)

type SubResolveT = GraphQLT SUBSCRIPTION


class PackT (o::OperationKind) m event where
    packT ::  Validation a -> GraphQLT o m event a

instance PackT QUERY m event where
instance PackT SUBSCRIPTION m event where
instance PackT MUTATION m event where

data GraphQLT (o::OperationKind) (m :: * -> * ) event value where
    QueryT:: ResolveT m value -> GraphQLT QUERY m  event value
    MutationT :: ResolveT (StreamT m event) value -> GraphQLT MUTATION m event value
    SubscriptionT ::  ResolveT (SubscribeStream m event) (event -> ResolveT m value) -> GraphQLT SUBSCRIPTION m event value
    FailT :: GQLErrors -> GraphQLT o m  event value

instance Functor m => Functor (GraphQLT o m e) where
    fmap _ (FailT mErrors) = FailT mErrors
    fmap f (QueryT mResolver) = QueryT $ fmap f mResolver
    fmap f (MutationT mResolver) = MutationT $ fmap f mResolver
    fmap f (SubscriptionT mResolver) = SubscriptionT (eventFmap <$> mResolver)
            where
                eventFmap res event = fmap f (res event)


class InitT (o::OperationKind) where
    initT :: Monad m => a -> GraphQLT o m event a

instance InitT QUERY where
   initT = QueryT . pure

instance InitT MUTATION where
   initT = MutationT . pure

instance InitT SUBSCRIPTION where
   initT = SubscriptionT . pure . const . pure

instance Monad m => Applicative (GraphQLT o m e) where
    --pure = initT
    -------------------------------------
    _ <*> (FailT mErrors) = FailT mErrors
    (FailT mErrors) <*> _ = FailT mErrors
    -------------------------------------
    (QueryT f) <*> (QueryT res) = QueryT (f <*> res)
    -------------------------------------
    (MutationT f) <*> (MutationT res) = MutationT (f <*> res)
    --------------------------------------------------------------
    (SubscriptionT f) <*> (SubscriptionT res) = SubscriptionT $ do
                       f1 <- f
                       res1 <- res
                       pure $ \event -> f1 event <*>  res1 event

unQueryT :: Applicative m => GraphQLT QUERY m e a -> ResolveT m a
unQueryT (QueryT x) = x
unQueryT (FailT x)  = ExceptT  $ pure $  Left x

unMutationT :: Applicative m => GraphQLT MUTATION m e a -> ResolveT (StreamT m e) a
unMutationT (MutationT x) = x
unMutationT (FailT x)     = ExceptT $ StreamT $ pure $ StreamState [] $ Left x

instance Monad m  => Monad (GraphQLT o m e) where
    return = pure
    (QueryT value) >>= nextM = QueryT (value >>= unQueryT . nextM)
    (MutationT value) >>= nextM = MutationT (value >>= unMutationT . nextM)



convertResolver :: Monad m => Position -> Key -> GADTResolver o m e a ->  GraphQLT o m e a
convertResolver position fieldName (FailedResolver message)       = FailT $ resolverError position fieldName message
convertResolver _ _ (MutationResolver events res) = MutationT $ ExceptT $ StreamT (StreamState events . Right <$> res)
    --FailT $ resolverError selectionPosition fieldName message
    --withRes (MutationResolver events res) = MutationT $ ExceptT $ StreamT (StreamState events . Right <$> res)
--  encode resolver selection =  handleResolver resolver
--    where
--      handleResolver (SubscriptionResolver subChannels subResolver) =
--        SubscriptionT $ initExceptStream [map Channel subChannels] ((encodeResolver selection . subResolver) :: event -> ResolveT m Value)
      --handleResolver (FailedResolving  errorMessage) = TODO: handle error

liftResolver :: (Monad m) => (a -> (Key,Selection) -> GraphQLT o m e value) -> (Key, Selection) -> GADTResolver o m e a  -> GraphQLT o m e value
liftResolver encode  selection@(fieldName, Selection {selectionPosition}) res = withRes res >>= (`encode` selection)
   where
    withRes :: Monad m => GADTResolver o m e a ->  GraphQLT o m e a
    withRes  = convertResolver selectionPosition fieldName

toResponseRes :: Monad m =>  GraphQLT o m event Value -> ResponseT m event Value
toResponseRes (FailT errors) = ExceptT $ StreamT $ pure $ StreamState [] $ Left errors
toResponseRes (QueryT resT) =  ExceptT $ StreamT $ StreamState [] <$> runExceptT resT
toResponseRes (MutationT resT) = ExceptT $ mapS Publish (runExceptT resT)
toResponseRes (SubscriptionT resT)  =
      ExceptT $ StreamT $ handleActions <$> closeStream (runExceptT resT)
      where
        handleActions (_, Left gqlError) = StreamState [] (Left gqlError)
        handleActions (channels, Right subResolver) =
          StreamState [Subscribe $ Event (concat channels) handleRes] (Right  gqlNull)
          where
            handleRes event = renderResponse <$> runExceptT (subResolver event)

data GADTResolver (o::OperationKind) (m :: * -> * ) event value where
    FailedResolver :: String -> GADTResolver o m event value
    QueryResolver:: ExceptT String m value -> GADTResolver QUERY m  event value
    MutationResolver :: [event] -> m value -> GADTResolver MUTATION m event value
    SubscriptionResolver :: [StreamChannel event] -> (event -> GADTResolver QUERY m  event value) -> GADTResolver SUBSCRIPTION m event value

instance Functor m => Functor (GADTResolver o m e) where
    fmap _ (FailedResolver mErrors) = FailedResolver mErrors
    fmap f (QueryResolver mResolver) = QueryResolver $ fmap f mResolver
    fmap f (MutationResolver events mResolver) = MutationResolver events $ fmap f mResolver
    fmap f (SubscriptionResolver events mResolver) = SubscriptionResolver events (eventFmap mResolver)
            where
                eventFmap res event = fmap f (res event)

instance Applicative m => Applicative (GADTResolver o m e)
instance Monad m => Monad (GADTResolver o m e)


type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (SubResolver m e) = GADTResolver QUERY m e

-------------------------------------------------------------------

failResolveT :: Monad m => GQLErrors -> ResolveT m a
failResolveT = ExceptT . pure . Left


-------------------------------------------------------------------
-- | Pure Resolver without effect
type Pure = Either String

-- | GraphQL Resolver
resolver :: m (Either String a) -> Resolver m a
resolver = ExceptT

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: GADTResolver QUERY m event (query (GADTResolver QUERY m  event))
  , mutationResolver     :: MutResolver m event (mut (MutResolver m event))
  , subscriptionResolver :: SubResolver m event (sub (SubResolver  m event))
  }
