{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Types.Internal.Resolver
  ( MutResolver
  , ResolveT
  , Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
  , GQLFail(..)
  , ResponseT
  , failResolveT
  , GADTResolver(..)
  , GraphQLT(..)
  , MapGraphQLT(..)
  , PureOperation(..)
  , resolveObject
  , resolveFields
  , toResponseRes
  , withObject
  , Resolving(..)
  ) where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT, withExceptT)
import           Data.Maybe                                 (fromMaybe)
import           Data.Text                                  (pack, unpack)
-- MORPHEUS
import           Data.Morpheus.Error.Selection              (resolverError, subfieldsNotSelected)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Base          (Message)
import           Data.Morpheus.Types.Internal.Data          (Key, MUTATION, OperationKind, QUERY, SUBSCRIPTION)
import           Data.Morpheus.Types.Internal.Stream        (Channel (..), Event (..), ResponseEvent (..),
                                                             ResponseStream, StreamChannel, StreamState (..),
                                                             StreamT (..), closeStream, injectEvents, mapS, pushEvents)
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Types.Internal.Value         (GQLValue (..), Value)
import           Data.Morpheus.Types.IO                     (renderResponse)

withObject :: ( SelectionSet -> GraphQLT o m e value) -> (Key,Selection)  -> GraphQLT o m e value
withObject f (_, Selection {selectionRec = SelectionSet selection}) = f selection
withObject _ (key, Selection {selectionPosition}) = FailT $ subfieldsNotSelected key "" selectionPosition

class Monad m =>
      GQLFail (t :: (* -> *) -> * -> *) m
  where
  gqlFail :: Monad m => Message -> t m a
  toSuccess :: Monad m => (Message -> b) -> (a -> b) -> t m a -> t m b

instance Monad m => GQLFail (ExceptT String) m where
  gqlFail = ExceptT . pure . Left . unpack
  toSuccess fFail fSuc (ExceptT value) = ExceptT $ pure . mapCases <$> value
    where
      mapCases (Right x) = fSuc x
      mapCases (Left x)  = fFail $ pack $ show x

----------------------------------------------------------------------------------------
type MutResolver = GADTResolver MUTATION


type ResolveT = ExceptT GQLErrors
type ResponseT m e  = ResolveT (ResponseStream m e)

--
-- Recursive Resolver

newtype RecResolver m a b = RecResolver {
  unRecResolver :: a -> ResolveT m b
}

instance Functor m => Functor (RecResolver m a) where
  fmap f (RecResolver x) = RecResolver eventFmap
    where
      eventFmap  event = fmap f (x event)

instance Monad m => Applicative (RecResolver m a) where
  pure = RecResolver . const . pure
  (RecResolver f) <*> (RecResolver res) = RecResolver recX
    where
      recX event = f event <*>  res event

instance Monad m => Monad (RecResolver m a) where
  (RecResolver x) >>= next = RecResolver recX
    where
        recX event = x event >>= (\v-> v event) . unRecResolver . next
------------------------------------------------------------
--
--- GraphQLT

data GraphQLT (o::OperationKind) (m:: * -> *) event value where
    QueryT:: { unQueryT :: ResolveT m value } -> GraphQLT QUERY m event value
    MutationT :: { unMutationT :: ResolveT (StreamT m event) value } -> GraphQLT MUTATION m event value
    SubscriptionT :: { unSubscriptionT :: ResolveT (StreamT m (Channel event)) (RecResolver m event value) } -> GraphQLT SUBSCRIPTION m event value
    -- TODO: SubscriptionRecT :: RecResolver m event value -> GraphQLT SUBSCRIPTION m event value
    FailT :: GQLErrors -> GraphQLT o m event  value

-- GraphQLT Functor
instance Monad m => Functor (GraphQLT o m e) where
    fmap _ (FailT mErrors)           = FailT mErrors
    fmap f (QueryT mResolver)        = QueryT $ f <$> mResolver
    fmap f (MutationT mResolver)     = MutationT $  f <$> mResolver
    fmap f (SubscriptionT mResolver) = SubscriptionT $ fmap f <$> mResolver

-- GraphQLT Applicative
instance (PureOperation o, Monad m) => Applicative (GraphQLT o m e) where
    pure = pureGraphQLT
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
                       pure (f1 <*> res1)

-- GADTResolver
data GADTResolver (o::OperationKind) (m :: * -> * ) event value where
    FailedResolver :: { unFailedResolver :: String } -> GADTResolver o m event value
    QueryResolver:: { unQueryResolver :: ExceptT String m value } -> GADTResolver QUERY m  event value
    MutationResolver :: { mutationResolverEvents :: [event] , mutationResolverValue :: ExceptT String m value } -> GADTResolver MUTATION m event value
    SubscriptionResolver :: {
            subscriptionResolverChannels :: [StreamChannel event] ,
            subscriptionResolverValue:: event -> GADTResolver QUERY m  event value
        } -> GADTResolver SUBSCRIPTION m event value

-- GADTResolver Functor
instance Functor m => Functor (GADTResolver o m e) where
    fmap _ (FailedResolver mErrors) = FailedResolver mErrors
    fmap f (QueryResolver mResolver) = QueryResolver $ fmap f mResolver
    fmap f (MutationResolver events mResolver) = MutationResolver events $ fmap f mResolver
    fmap f (SubscriptionResolver events mResolver) = SubscriptionResolver events (eventFmap mResolver)
            where
                eventFmap res event = fmap f (res event)

-- GADTResolver Applicative
instance (PureOperation o ,Monad m) => Applicative (GADTResolver o m e) where
    pure = pureRes
    -------------------------------------
    _ <*> (FailedResolver mErrors) = FailedResolver mErrors
    (FailedResolver mErrors) <*> _ = FailedResolver mErrors
    -------------------------------------
    (QueryResolver f) <*> (QueryResolver res) = QueryResolver (f <*> res)
    ---------------------------------------------------------------------
    (MutationResolver events1 f) <*> (MutationResolver events2 res) = MutationResolver (events1 <> events2) (f <*> res)
    --------------------------------------------------------------
    (SubscriptionResolver e1 f) <*> (SubscriptionResolver e2 res) = SubscriptionResolver (e1<>e2) $
                       \event -> f event <*>  res event

instance (Monad m) => Monad (GADTResolver QUERY m e) where
    return = pure
    -------------------------------------
    (FailedResolver mErrors) >>= _ = FailedResolver mErrors
    -------------------------------------
    (QueryResolver f) >>= nextM = QueryResolver (f >>= unQueryResolver. nextM)

-- Pure Operation
class PureOperation (o::OperationKind) where
    pureRes :: Monad m => a -> GADTResolver o m event a
    pureGraphQLT :: Monad m => a -> GraphQLT o m event a
    eitherGraphQLT :: Monad m => Validation a -> GraphQLT o m event a

instance PureOperation QUERY where
   pureRes = QueryResolver . pure
   pureGraphQLT = QueryT . pure
   eitherGraphQLT = QueryT . ExceptT . pure

instance PureOperation MUTATION where
   pureRes = MutationResolver [] . pure
   pureGraphQLT = MutationT . pure
   eitherGraphQLT = MutationT . ExceptT . pure

instance PureOperation SUBSCRIPTION where
   pureRes = SubscriptionResolver []  . const . pure
   pureGraphQLT = SubscriptionT . pure . pure
   eitherGraphQLT = SubscriptionT . fmap pure  . ExceptT . pure

resolveObject :: (Monad m , PureOperation o ) => SelectionSet -> [FieldRes o m e] -> GraphQLT o m e Value
resolveObject selSet = fmap gqlObject . resolveFields selSet

resolveFields :: (Monad m , PureOperation o ) => SelectionSet -> [FieldRes o m e] -> GraphQLT o m e [(Key,Value)]
resolveFields selectionSet resolvers = traverse selectResolver selectionSet
  where
    selectResolver (key, selection) =
      (key, ) <$>
      case selectionRec selection of
        SelectionAlias name selectionRec -> lookupRes name (selection {selectionRec})
        _                                -> lookupRes key selection
        -------------------------------------------------------------
      where
        lookupRes resKey sel = (fromMaybe (const $ pure  gqlNull) $ lookup resKey resolvers) (key, sel)

class Resolving o m e where
     resolvingOperation :: (PureOperation o ,Monad m) => [FieldRes o m e] -> (Key,Selection) -> GraphQLT o m e [(Key,Value)]
     getArgs :: Validation args ->  (args -> GADTResolver o m e value) -> GADTResolver o m e value
     resolving :: Monad m => (value -> (Key,Selection) -> GraphQLT o m e Value) -> GADTResolver o m e value ->  (Key,Selection) -> GraphQLT o m e Value

type FieldRes o m e = (Key, (Key, Selection) -> GraphQLT o m e Value)

instance Resolving o m e where
   getArgs (Right x) f = f x
   getArgs (Left _) _  = FailedResolver ""
   ------------------------------------------
   resolvingOperation resolvers = withObject resObj
     where
        resObj selectionSet = resolveFields selectionSet resolvers
   ---------------------------------------------------------------------------------------------------------------------------------------
   resolving encode gResolver selection@(fieldName,Selection { selectionPosition }) = __resolving gResolver
        where
          __resolving (FailedResolver message) = FailT $ resolverError selectionPosition fieldName message
          __resolving (QueryResolver res) =
            QueryT $ withExceptT (resolverError selectionPosition fieldName) res >>= unQueryT . (`encode` selection)
   ---------------------------------------------------------------------------------------------------------------------------------------
          __resolving (MutationResolver events res)  =
            MutationT $ pushEvents events $ withExceptT (resolverError selectionPosition fieldName) (injectEvents [] res)  >>= unMutationT . (`encode` selection)
   --------------------------------------------------------------------------------------------------------------------------------
          __resolving (SubscriptionResolver subChannels res) =
               SubscriptionT $ ExceptT $ StreamT $ pure $ StreamState { streamEvents , streamValue }
                              where
                                streamValue  = pure $ RecResolver $ \event -> withExceptT (resolverError selectionPosition fieldName) ( unQueryResolver $ res event)  >>= unPub event . (`encode` selection)
                                streamEvents :: [Channel e]
                                streamEvents = map Channel subChannels

unPub :: Monad m => event -> GraphQLT SUBSCRIPTION m event a -> ResolveT m a
unPub event x = do
    func <- unPureSub x
    func event

unPureSub :: Monad m => GraphQLT SUBSCRIPTION m event a -> ResolveT m (event -> ResolveT m a)
unPureSub = ExceptT . fmap (fmap unRecResolver . streamValue) . runStreamT . runExceptT . unSubscriptionT

class MapGraphQLT (fromO :: OperationKind) (toO :: OperationKind) where
   mapGraphQLT :: Monad m => GraphQLT fromO m e a -> GraphQLT toO m e a

instance MapGraphQLT fromO fromO where
    mapGraphQLT = id

instance MapGraphQLT QUERY SUBSCRIPTION where
    mapGraphQLT (QueryT x) = SubscriptionT $ injectEvents [] (fmap pure x)
    mapGraphQLT (FailT x)  = FailT x

toResponseRes :: Monad m =>  GraphQLT o m event Value -> ResponseT m event Value
toResponseRes (FailT errors) = ExceptT $ StreamT $ pure $ StreamState [] $ Left errors
toResponseRes (QueryT resT) =  ExceptT $ StreamT $ StreamState [] <$> runExceptT resT
toResponseRes (MutationT resT) = ExceptT $ mapS Publish (runExceptT resT)
toResponseRes (SubscriptionT resT)  =
      ExceptT $ StreamT $ handleActions <$> closeStream (runExceptT resT)
      where
        handleActions (_, Left gqlError) = StreamState [] (Left gqlError)
        handleActions (channels, Right subResolver) =
          StreamState [Subscribe $ Event channels handleRes] (Right  gqlNull)
          where
            handleRes event = renderResponse <$> runExceptT (unRecResolver subResolver event)

type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (GADTResolver SUBSCRIPTION m e) = GADTResolver QUERY m e

-------------------------------------------------------------------
failResolveT :: Monad m => GQLErrors -> ResolveT m a
failResolveT = ExceptT . pure . Left

-------------------------------------------------------------------
-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: query (GADTResolver QUERY m  event)
  , mutationResolver     :: mut (GADTResolver MUTATION m event)
  , subscriptionResolver :: sub (GADTResolver SUBSCRIPTION  m event)
  }
