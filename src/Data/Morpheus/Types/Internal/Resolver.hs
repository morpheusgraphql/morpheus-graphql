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
  , LiftEither(..)
  , resolveObject
  , toResponseRes
  , withObject
  , resolving
  , getArgs
  , lift
  )
where

import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT
                                                , withExceptT
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( unpack )
import           Control.Monad                  ( (>=>) )

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
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Message )
import           Data.Morpheus.Types.Internal.AST.Data
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
                                                , toStream
                                                , injectEvents
                                                , mapS
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLErrors
                                                , Validation
                                                , Computation(..)
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( GQLValue(..)
                                                , Value
                                                )
import           Data.Morpheus.Types.IO         ( renderResponse )


class LiftEither (o::OperationType) res where
  type ResError res :: *
  liftEither :: Monad m => m (Either (ResError res) a) -> res o event m  a

type ResolveT = ExceptT GQLErrors
type ResponseT m e = ResolveT (ResponseStream e m)
----------------------------------------------------------------------------------------
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

-- Functor
instance Monad m => Functor (ResolvingStrategy o e m) where
  fmap f (QueryResolving        res) = QueryResolving $ f <$> res
  fmap f (MutationResolving     res) = MutationResolving $ f <$> res
  fmap f (SubscriptionResolving res) = SubscriptionResolving $ (<$>) f <$> res

-- Applicative
instance (LiftEither o ResolvingStrategy, Monad m) => Applicative (ResolvingStrategy o e m) where
  pure = liftEither . pure . pure
  -------------------------------------
  (QueryResolving f) <*> (QueryResolving res) = QueryResolving (f <*> res)
  ------------------------------------------------------------------------
  (MutationResolving f) <*> (MutationResolving res) =
    MutationResolving (f <*> res)
  --------------------------------------------------------------
  (SubscriptionResolving f) <*> (SubscriptionResolving res) =
    SubscriptionResolving $ (<*>) <$> f <*> res

-- LiftEither
instance LiftEither QUERY ResolvingStrategy where
  type ResError ResolvingStrategy = GQLErrors
  liftEither = QueryResolving . ExceptT

instance LiftEither MUTATION ResolvingStrategy where
  type ResError ResolvingStrategy = GQLErrors
  liftEither = MutationResolving . ExceptT . StreamT . fmap (StreamState [])

instance LiftEither SUBSCRIPTION ResolvingStrategy where
  type ResError ResolvingStrategy = GQLErrors
  liftEither = SubscriptionResolving . ExceptT . StreamT . fmap
    (StreamState [] . fmap pure)

 -- Failure
instance (LiftEither o ResolvingStrategy, Monad m) => Failure GQLErrors (ResolvingStrategy o e m) where
  failure = liftEither . pure . Left

-- helper functins
withObject
  :: (LiftEither o ResolvingStrategy, Monad m)
  => (SelectionSet -> ResolvingStrategy o e m value)
  -> (Key, ValidSelection)
  -> ResolvingStrategy o e m value
withObject f (_, Selection { selectionRec = SelectionSet selection }) =
  f selection
withObject _ (key, Selection { selectionPosition }) =
  failure (subfieldsNotSelected key "" selectionPosition)

resolveObject
  :: (Monad m, LiftEither o ResolvingStrategy)
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

toResponseRes
  :: Monad m => ResolvingStrategy o event m Value -> ResponseT event m Value
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
    where handleRes event = renderResponse (unRecResolver subResolver event)


--
--     
-- GraphQL Field Resolver
--
--
---------------------------------------------------------------
data Resolver (o::OperationType) event (m :: * -> * )  value where
    QueryResolver::{ unQueryResolver :: ExceptT String m value } -> Resolver QUERY   event m value
    MutResolver ::{ unMutResolver :: ExceptT String m ([event],value) } -> Resolver MUTATION event m  value
    SubResolver ::{
            subChannels :: [StreamChannel event] ,
            subResolver :: event -> Resolver QUERY event m value
        } -> Resolver SUBSCRIPTION event m  value

-- Functor
instance Functor m => Functor (Resolver o e m) where
  fmap f (QueryResolver res) = QueryResolver $ fmap f res
  fmap f (MutResolver   res) = MutResolver $ tupleMap <$> res
    where tupleMap (e, v) = (e, f v)
  fmap f (SubResolver events mResolver) = SubResolver events
                                                      (eventFmap mResolver)
    where eventFmap res event = fmap f (res event)

-- Applicative
instance (LiftEither o Resolver ,Monad m) => Applicative (Resolver o e m) where
  pure = liftEither . pure . pure
  -------------------------------------
  (QueryResolver f) <*> (QueryResolver res) = QueryResolver (f <*> res)
  ---------------------------------------------------------------------
  MutResolver res1 <*> MutResolver res2 = MutResolver $ join <$> res1 <*> res2
    where join (e1, f) (e2, v) = (e1 <> e2, f v)
  --------------------------------------------------------------
  (SubResolver e1 f) <*> (SubResolver e2 res) = SubResolver (e1 <> e2) subRes
    where subRes event = f event <*> res event

-- Monad 
instance (Monad m) => Monad (Resolver QUERY e m) where
  return = pure
  -----------------------------------------------------
  (QueryResolver f) >>= nextM = QueryResolver (f >>= unQueryResolver . nextM)

instance (Monad m) => Monad (Resolver MUTATION e m) where
  return = pure
  -----------------------------------------------------
  (MutResolver m1) >>= mFunc = MutResolver $ do
    (e1, v1) <- m1
    (e2, v2) <- unMutResolver $ mFunc v1
    pure (e1 <> e2, v2)

-- Monad Transformers    
instance MonadTrans (Resolver QUERY e) where
  lift = liftEither . fmap pure

instance MonadTrans (Resolver MUTATION e) where
  lift = liftEither . fmap pure

-- LiftEither
instance LiftEither QUERY Resolver where
  type ResError Resolver = String
  liftEither = QueryResolver . ExceptT

instance LiftEither MUTATION Resolver where
  type ResError Resolver = String
  liftEither = MutResolver . fmap ([], ) . ExceptT

instance LiftEither SUBSCRIPTION Resolver where
  type ResError Resolver = String
  liftEither = SubResolver [] . const . liftEither

-- Failure
instance (LiftEither o Resolver, Monad m) => Failure Message (Resolver o e m) where
  failure = liftEither . pure . Left . unpack

-- Type Helpers  
type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (Resolver SUBSCRIPTION m e) = Resolver QUERY m e


-- RESOLVING

type FieldRes o e m
  = (Key, (Key, ValidSelection) -> ResolvingStrategy o e m Value)

getArgs
  :: (LiftEither o Resolver, Monad m)
  => Validation args
  -> (args -> Resolver o e m value)
  -> Resolver o e m value
getArgs (Success args _) f = f args
getArgs (Failure errors) _ = failure ("TODO: errors" :: Message)

resolving
  :: Monad m
  => (value -> (Key, ValidSelection) -> ResolvingStrategy o e m Value)
  -> Resolver o e m value
  -> (Key, ValidSelection)
  -> ResolvingStrategy o e m Value
resolving encode gResolver selection@(fieldName, Selection { selectionPosition })
  = _resolve gResolver
 where
  convert :: Functor m => ExceptT String m a -> ExceptT GQLErrors m a
  convert = withExceptT (resolverError selectionPosition fieldName)
  ------------------------------
  _encode = (`encode` selection)
  -------------------------------------------------------------------
  _resolve (QueryResolver res) =
    QueryResolving $ convert res >>= unQueryT . _encode
  ---------------------------------------------------------------------------------------------------------------------------------------
  _resolve (MutResolver res) =
    MutationResolving $ convert (toStream res) >>= unMutationT . _encode
  --------------------------------------------------------------------------------------------------------------------------------
  _resolve (SubResolver subChannels res) =
    SubscriptionResolving $ ExceptT $ StreamT $ pure $ StreamState
      { streamEvents = map Channel subChannels
      , streamValue  = pure $ RecResolver resolveSub
      }
   where
    resolveSub event =
      convert (unQueryResolver $ res event) >>= toExceptT . _encode
     where
      withEvent f = f event
      ---------------------------------------------------------
      toExceptT = unPureSub >=> withEvent
      -----------------------------------
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

-------------------------------------------------------------------
-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: query (Resolver QUERY event m)
  , mutationResolver     :: mut (Resolver MUTATION event m)
  , subscriptionResolver :: sub (Resolver SUBSCRIPTION  event m)
  }
