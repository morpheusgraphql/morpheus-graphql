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
  ( ResolveT
  , Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
  , ResponseT
  , Resolver(..)
  , ResolvingStrategy(..)
  , MapGraphQLT(..)
  , PureOperation(..)
  , resolveObject
  , toResponseRes
  , withObject
  , Resolving(..)
  , lift
  )
where

import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Fail             ( MonadFail(..) )
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT
                                                , withExceptT
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )

-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( resolverError
                                                , subfieldsNotSelected
                                                )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( Selection(..)
                                                , SelectionRec(..)
                                                , SelectionSet
                                                , ValidSelection
                                                )
import           Data.Morpheus.Types.Internal.Data
                                                ( Key
                                                , MUTATION
                                                , OperationType
                                                , QUERY
                                                , SUBSCRIPTION
                                                )
import           Data.Morpheus.Types.Internal.Stream
                                                ( Channel(..)
                                                , Event(..)
                                                , ResponseEvent(..)
                                                , ResponseStream
                                                , StreamChannel
                                                , StreamState(..)
                                                , StreamT(..)
                                                , closeStream
                                                , injectEvents
                                                , mapS
                                                , pushEvents
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLErrors
                                                , Validation
                                                )
import           Data.Morpheus.Types.Internal.Value
                                                ( GQLValue(..)
                                                , Value
                                                )
import           Data.Morpheus.Types.IO         ( renderResponse )

withObject
  :: (SelectionSet -> ResolvingStrategy o e m value)
  -> (Key, ValidSelection)
  -> ResolvingStrategy o e m value
withObject f (_, Selection { selectionRec = SelectionSet selection }) =
  f selection
withObject _ (key, Selection { selectionPosition }) =
  Fail $ subfieldsNotSelected key "" selectionPosition

instance MonadTrans (Resolver QUERY e) where
  lift = liftEither . fmap pure

instance MonadTrans (Resolver MUTATION e) where
  lift = liftEither . fmap pure

----------------------------------------------------------------------------------------
type ResolveT = ExceptT GQLErrors
type ResponseT m e = ResolveT (ResponseStream e m)

instance Monad m => MonadFail (Resolver QUERY e m) where
  fail = FailedResolver

--
-- Recursive Resolver
newtype RecResolver m a b = RecResolver {
  unRecResolver :: a -> ResolveT m b
}

instance Functor m => Functor (RecResolver m a) where
  fmap f (RecResolver x) = RecResolver eventFmap
    where eventFmap event = fmap f (x event)

instance Monad m => Applicative (RecResolver m a) where
  pure = RecResolver . const . pure
  (RecResolver f) <*> (RecResolver res) = RecResolver recX
    where recX event = f event <*> res event

instance Monad m => Monad (RecResolver m a) where
  (RecResolver x) >>= next = RecResolver recX
    where recX event = x event >>= (\v -> v event) . unRecResolver . next
------------------------------------------------------------
--
--- GraphQLT

data ResolvingStrategy  (o::OperationType) event (m:: * -> *) value where
    QueryResolving ::{ unQueryT :: ResolveT m value } -> ResolvingStrategy QUERY event m  value
    MutationResolving ::{ unMutationT :: ResolveT (StreamT m event) value } -> ResolvingStrategy MUTATION event m  value
    SubscriptionResolving ::{ unSubscriptionT :: ResolveT (StreamT m (Channel event)) (RecResolver m event value) } -> ResolvingStrategy SUBSCRIPTION event m value
    -- SubscriptionRecT :: RecResolver m event value -> GraphQLT SUBSCRIPTION m event value
    Fail ::GQLErrors -> ResolvingStrategy o e mvent  value

-- GraphQLT Functor
instance Monad m => Functor (ResolvingStrategy o e m) where
  fmap _ (Fail              mErrors  ) = Fail mErrors
  fmap f (QueryResolving    mResolver) = QueryResolving $ f <$> mResolver
  fmap f (MutationResolving mResolver) = MutationResolving $ f <$> mResolver
  fmap f (SubscriptionResolving mResolver) =
    SubscriptionResolving $ fmap f <$> mResolver

-- GraphQLT Applicative
instance (PureOperation o, Monad m) => Applicative (ResolvingStrategy o e m) where
  pure = pureGraphQLT
  -------------------------------------
  _                        <*> (Fail mErrors)       = Fail mErrors
  (Fail           mErrors) <*> _                    = Fail mErrors
  -------------------------------------
  (QueryResolving f      ) <*> (QueryResolving res) = QueryResolving (f <*> res)
  ------------------------------------------------------------------------
  (MutationResolving f) <*> (MutationResolving res) =
    MutationResolving (f <*> res)
  --------------------------------------------------------------
  (SubscriptionResolving f) <*> (SubscriptionResolving res) =
    SubscriptionResolving $ do
      f1   <- f
      res1 <- res
      pure (f1 <*> res1)

-- GADTResolver
---------------------------------------------------------------
data Resolver (o::OperationType) event (m :: * -> * )  value where
    FailedResolver ::{ unFailedResolver :: String } -> Resolver o  event m value
    QueryResolver::{ unQueryResolver :: ExceptT String m value } -> Resolver QUERY   event m value
    MutResolver ::{
            mutEvents :: [event] ,
            mutResolver :: ExceptT String m value
        } -> Resolver MUTATION event m  value
    SubResolver ::{
            subChannels :: [StreamChannel event] ,
            subResolver :: event -> Resolver QUERY event m value
        } -> Resolver SUBSCRIPTION event m  value

-- GADTResolver Functor
instance Functor m => Functor (Resolver o e m) where
  fmap _ (FailedResolver mErrors      ) = FailedResolver mErrors
  fmap f (QueryResolver  mResolver    ) = QueryResolver $ fmap f mResolver
  fmap f (MutResolver events mResolver) = MutResolver events $ fmap f mResolver
  fmap f (SubResolver events mResolver) = SubResolver events
                                                      (eventFmap mResolver)
    where eventFmap res event = fmap f (res event)

-- GADTResolver Applicative
instance (PureOperation o ,Monad m) => Applicative (Resolver o e m) where
  pure = liftEither . pure . pure
  -------------------------------------
  _ <*> (FailedResolver mErrors) = FailedResolver mErrors
  (FailedResolver mErrors) <*> _ = FailedResolver mErrors
  -------------------------------------
  (QueryResolver f) <*> (QueryResolver res) = QueryResolver (f <*> res)
  ---------------------------------------------------------------------
  (MutResolver events1 f) <*> (MutResolver events2 res) =
    MutResolver (events1 <> events2) (f <*> res)
  --------------------------------------------------------------
  (SubResolver e1 f) <*> (SubResolver e2 res) =
    SubResolver (e1 <> e2) $ \event -> f event <*> res event

instance (Monad m) => Monad (Resolver QUERY e m) where
  return = pure
  -------------------------------------
  (FailedResolver mErrors) >>= _ = FailedResolver mErrors
  -------------------------------------
  (QueryResolver f) >>= nextM = QueryResolver (f >>= unQueryResolver . nextM)

-- Pure Operation
class PureOperation (o::OperationType) where
    liftEither :: Monad m => m (Either String a) -> Resolver o event m  a
    pureGraphQLT :: Monad m => a -> ResolvingStrategy o  event m a
    eitherGraphQLT :: Monad m => Validation a -> ResolvingStrategy o event m a

instance PureOperation QUERY where
  liftEither     = QueryResolver . ExceptT
  pureGraphQLT   = QueryResolving . pure
  eitherGraphQLT = QueryResolving . ExceptT . pure

instance PureOperation MUTATION where
  liftEither     = MutResolver [] . ExceptT
  pureGraphQLT   = MutationResolving . pure
  eitherGraphQLT = MutationResolving . ExceptT . pure

instance PureOperation SUBSCRIPTION where
  liftEither     = SubResolver [] . const . liftEither
  pureGraphQLT   = SubscriptionResolving . pure . pure
  eitherGraphQLT = SubscriptionResolving . fmap pure . ExceptT . pure


resolveObject
  :: (Monad m, PureOperation o)
  => SelectionSet
  -> [FieldRes o e m]
  -> ResolvingStrategy o e m Value
resolveObject selectionSet fieldResolvers =
  gqlObject <$> traverse selectResolver selectionSet
 where
  selectResolver (key, selection@Selection { selectionAlias }) =
    (fromMaybe key selectionAlias, ) <$> lookupRes selection
   where
    lookupRes sel =
      (fromMaybe (const $ pure gqlNull) $ lookup key fieldResolvers) (key, sel)

class Resolving o e m where
     getArgs :: Validation args ->  (args -> Resolver o e m value) -> Resolver o e m  value
     resolving :: Monad m => (value -> (Key,ValidSelection) -> ResolvingStrategy o  e m Value) -> Resolver o e m value ->  (Key, ValidSelection) -> ResolvingStrategy o e m Value

type FieldRes o e m
  = (Key, (Key, ValidSelection) -> ResolvingStrategy o e m Value)

instance Resolving o e m  where
  getArgs (Right x) f = f x
  getArgs (Left  _) _ = FailedResolver ""
  ---------------------------------------------------------------------------------------------------------------------------------------
  resolving encode gResolver selection@(fieldName, Selection { selectionPosition })
    = __resolving gResolver
   where
    __resolving (FailedResolver message) =
      Fail $ resolverError selectionPosition fieldName message
    __resolving (QueryResolver res) =
      QueryResolving
        $   withExceptT (resolverError selectionPosition fieldName) res
        >>= unQueryT
        .   (`encode` selection)
---------------------------------------------------------------------------------------------------------------------------------------
    __resolving (MutResolver events res) =
      MutationResolving
        $   pushEvents events
        $   withExceptT (resolverError selectionPosition fieldName)
                        (injectEvents [] res)
        >>= unMutationT
        .   (`encode` selection)
--------------------------------------------------------------------------------------------------------------------------------
    __resolving (SubResolver subChannels res) =
      SubscriptionResolving $ ExceptT $ StreamT $ pure $ StreamState
        { streamEvents
        , streamValue
        }
     where
      streamValue = pure $ RecResolver $ \event ->
        withExceptT (resolverError selectionPosition fieldName)
                    (unQueryResolver $ res event)
          >>= unPub event
          .   (`encode` selection)
      streamEvents :: [Channel e]
      streamEvents = map Channel subChannels

unPub
  :: Monad m
  => event
  -> ResolvingStrategy SUBSCRIPTION event m a
  -> ResolveT m a
unPub event x = do
  func <- unPureSub x
  func event

unPureSub
  :: Monad m
  => ResolvingStrategy SUBSCRIPTION event m a
  -> ResolveT m (event -> ResolveT m a)
unPureSub =
  ExceptT
    . fmap (fmap unRecResolver . streamValue)
    . runStreamT
    . runExceptT
    . unSubscriptionT

class MapGraphQLT (fromO :: OperationType) (toO :: OperationType) where
   mapGraphQLT :: Monad m => ResolvingStrategy fromO e m a -> ResolvingStrategy toO e m a

instance MapGraphQLT fromO fromO where
  mapGraphQLT = id

instance MapGraphQLT QUERY SUBSCRIPTION where
  mapGraphQLT (QueryResolving x) =
    SubscriptionResolving $ injectEvents [] (fmap pure x)
  mapGraphQLT (Fail x) = Fail x

toResponseRes
  :: Monad m => ResolvingStrategy o event m Value -> ResponseT event m Value
toResponseRes (Fail errors) =
  ExceptT $ StreamT $ pure $ StreamState [] $ Left errors
toResponseRes (QueryResolving resT) =
  ExceptT $ StreamT $ StreamState [] <$> runExceptT resT
toResponseRes (MutationResolving resT) =
  ExceptT $ mapS Publish (runExceptT resT)
toResponseRes (SubscriptionResolving resT) =
  ExceptT $ StreamT $ handleActions <$> closeStream (runExceptT resT)
 where
  handleActions (_       , Left gqlError    ) = StreamState [] (Left gqlError)
  handleActions (channels, Right subResolver) = StreamState
    [Subscribe $ Event channels handleRes]
    (Right gqlNull)
   where
    handleRes event =
      renderResponse <$> runExceptT (unRecResolver subResolver event)

type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (Resolver SUBSCRIPTION m e) = Resolver QUERY m e

-------------------------------------------------------------------
-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: query (Resolver QUERY event m)
  , mutationResolver     :: mut (Resolver MUTATION event m)
  , subscriptionResolver :: sub (Resolver SUBSCRIPTION  event m)
  }
