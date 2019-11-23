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
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Types.Internal.Resolving.Resolver
  ( ResolveT
  , Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
  , ResponseT
  , Resolver(..)
  , ResolvingStrategy(..)
  , MapStrategy(..)
  , LiftEither(..)
  , resolveObject
  , toResponseRes
  , withObject
  , resolving
  , toResolver
  , lift
  , SubEvent
  , GQLChannel(..)
  , ResponseEvent(..)
  , ResponseStream
  )
where

import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( unpack
                                                , pack
                                                )

-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( resolvingFailedError
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
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( GQLErrors
                                                , GQLError
                                                , Validation
                                                , Result(..)
                                                , Failure(..)
                                                , ResultT(..)
                                                , fromEither
                                                , mapExceptGQL
                                                , fromEitherSingle
                                                , restartEvents
                                                , mapFailure
                                                , mapEvent
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( GQLValue(..)
                                                , Value
                                                )
import           Data.Morpheus.Types.IO         ( renderResponse2
                                                , GQLResponse
                                                )
-- MORPHEUS


class LiftEither (o::OperationType) res where
  type ResError res :: *
  liftEither :: Monad m => m (Either (ResError res) a) -> res o event m  a

type ResolveT = ExceptT GQLErrors
type ResponseT m e = ResponseStream m e
----------------------------------------------------------------------------------------
-- Recursive Resolver
newtype RecResolver m a b = RecResolver {
  unRecResolver :: a -> ResolveT m b
}

instance Functor m => Functor (RecResolver m a) where
  fmap f (RecResolver x) = RecResolver recX where recX event = f <$> x event

instance Monad m => Applicative (RecResolver m a) where
  pure = RecResolver . const . pure
  (RecResolver f) <*> (RecResolver res) = RecResolver recX
    where recX event = f event <*> res event

instance Monad m => Monad (RecResolver m a) where
  (RecResolver x) >>= next = RecResolver recX
    where recX event = x event >>= (\v -> v event) . unRecResolver . next
------------------------------------------------------------

--- GraphQLT
data ResolvingStrategy  (o::OperationType) event (m:: * -> *) value where
  ResolveQ ::{ unResolveQ :: ResultT () GQLError 'True m value } -> ResolvingStrategy QUERY event m  value
  ResolveM ::{ unResolveM :: ResultT event GQLError 'True m value } -> ResolvingStrategy MUTATION event m  value
  ResolveS ::{ unResolveS :: ResultT (Channel event) GQLError 'True m (RecResolver m event value) } -> ResolvingStrategy SUBSCRIPTION event m value

-- Functor
instance Monad m => Functor (ResolvingStrategy o e m) where
  fmap f (ResolveQ res) = ResolveQ $ f <$> res
  fmap f (ResolveM res) = ResolveM $ f <$> res
  fmap f (ResolveS res) = ResolveS $ (<$>) f <$> res

-- Applicative
instance (LiftEither o ResolvingStrategy, Monad m) => Applicative (ResolvingStrategy o e m) where
  pure = liftEither . pure . pure
  -------------------------------------
  (ResolveQ f) <*> (ResolveQ res) = ResolveQ (f <*> res)
  ------------------------------------------------------------------------
  (ResolveM f) <*> (ResolveM res) = ResolveM (f <*> res)
  --------------------------------------------------------------
  (ResolveS f) <*> (ResolveS res) = ResolveS $ (<*>) <$> f <*> res

-- LiftEither
instance LiftEither QUERY ResolvingStrategy where
  type ResError ResolvingStrategy = GQLErrors
  liftEither = ResolveQ . ResultT . fmap fromEither

instance LiftEither MUTATION ResolvingStrategy where
  type ResError ResolvingStrategy = GQLErrors
  liftEither = ResolveM . ResultT . fmap fromEither

instance LiftEither SUBSCRIPTION ResolvingStrategy where
  type ResError ResolvingStrategy = GQLErrors
  liftEither = ResolveS . ResultT . fmap (fromEither . fmap pure)

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
toResponseRes (ResolveQ resT) = restartEvents resT
toResponseRes (ResolveM resT) = mapEvent Publish resT
toResponseRes (ResolveS resT) = ResultT $ handleActions <$> runResultT resT
 where
  handleActions (Failure gqlError                ) = Failure gqlError
  handleActions (Success subRes warnings channels) = Success
    { result   = gqlNull
    , warnings
    , events   = [Subscribe $ Event channels eventResolver]
    }
    where eventResolver event = renderResponse2 (unRecResolver subRes event)

--
--     
-- GraphQL Field Resolver
--
--
--ResultT  GQLError 'True m (RecResolver m event value)
---------------------------------------------------------------
data Resolver (o::OperationType) event (m :: * -> * )  value where
    QueryResolver::{ unQueryResolver :: ResultT () String 'True m value } -> Resolver QUERY   event m value
    MutResolver ::{ unMutResolver :: ResultT event String 'True m ([event],value) } -> Resolver MUTATION event m  value
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
  liftEither = QueryResolver . ResultT . fmap fromEitherSingle

instance LiftEither MUTATION Resolver where
  type ResError Resolver = String
  liftEither = MutResolver . ResultT . fmap (fromEitherSingle . fmap ([], ))

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

toResolver
  :: (LiftEither o Resolver, Monad m)
  => Validation a
  -> (a -> Resolver o e m b)
  -> Resolver o e m b
toResolver Success { result } f = f result
toResolver (Failure errors) _ =
  failure ("TODO: errors" <> pack (show errors) :: Message)

resolving
  :: forall o e m value
   . Monad m
  => (value -> (Key, ValidSelection) -> ResolvingStrategy o e m Value)
  -> Resolver o e m value
  -> (Key, ValidSelection)
  -> ResolvingStrategy o e m Value
resolving encode gResolver selection@(fieldName, Selection { selectionPosition })
  = _resolve gResolver
 where
  convert :: Monad m => ResultT ev String con m a -> ResultT ev GQLError con m a
  convert =
    mapFailure (resolvingFailedError selectionPosition fieldName . pack)
  ------------------------------
  _encode = (`encode` selection)
  -------------------------------------------------------------------
  _resolve (QueryResolver res) =
    ResolveQ $ convert res >>= unResolveQ . _encode
  ---------------------------------------------------------------------------------------------------------------------------------------
  _resolve (MutResolver res) =
    ResolveM $ replace (convert res) >>= unResolveM . _encode
   where
    replace (ResultT mx) = ResultT $ do
      value <- mx
      pure $ case value of
        Success { warnings, events, result = (events2, result) } ->
          Success { result, warnings, events = events <> events2 }
        Failure x -> Failure x
  --------------------------------------------------------------------------------------------------------------------------------
  _resolve (SubResolver subChannels res) = ResolveS $ ResultT $ pure $ Success
    { events   = map Channel subChannels
    , result   = RecResolver eventResolver
    , warnings = []
    }
   where
    eventResolver :: e -> ResolveT m Value
    eventResolver event =
      mapExceptGQL (convert $ unQueryResolver $ res event)
      >>= unPureSub
      .   _encode
     where
      unPureSub
        :: Monad m
        => ResolvingStrategy SUBSCRIPTION e m Value
        -> ResolveT m Value
      unPureSub (ResolveS (ResultT x)) = ExceptT (x >>= passEvent)
       where
        passEvent
          :: Result (Channel e) 'True GQLError (RecResolver m e Value)
          -> m (Either GQLErrors Value)
        passEvent Success { result = (RecResolver f) } = runExceptT (f event)
        passEvent (Failure er)                         = pure (Left er)


-- map Resolving strategies 
class MapStrategy (from :: OperationType) (to :: OperationType) where
   mapStrategy :: Monad m => ResolvingStrategy from e m a -> ResolvingStrategy to e m a

instance MapStrategy o o where
  mapStrategy = id

instance MapStrategy QUERY SUBSCRIPTION where
  mapStrategy (ResolveQ x) = ResolveS $ pure <$> restartEvents x

-------------------------------------------------------------------
-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: query (Resolver QUERY event m)
  , mutationResolver     :: mut (Resolver MUTATION event m)
  , subscriptionResolver :: sub (Resolver SUBSCRIPTION  event m)
  }

 -- EVENTS
data ResponseEvent m event
  = Publish event
  | Subscribe (SubEvent m event)

-- Channel

newtype Channel event = Channel {
  unChannel :: StreamChannel event
}

instance (Eq (StreamChannel event)) => Eq (Channel event) where
  Channel x == Channel y = x == y

class GQLChannel a where
  type StreamChannel a :: *
  streamChannels :: a -> [Channel a]

instance GQLChannel () where
  type StreamChannel () = ()
  streamChannels _ = []

instance GQLChannel (Event channel content)  where
  type StreamChannel (Event channel content) = channel
  streamChannels Event { channels } = map Channel channels

data Event e c = Event
  { channels :: [e], content  :: c}


type ResponseStream event m = ResultT (ResponseEvent m event) GQLError 'True m

type SubEvent m event = Event (Channel event) (event -> m GQLResponse)
  