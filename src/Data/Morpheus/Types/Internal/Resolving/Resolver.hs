{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Morpheus.Types.Internal.Resolving.Resolver
  ( Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
  , Resolver(..)
  , MapStrategy(..)
  , LiftOperation
  , resolveObject
  , resolveEnum
  , toResponseRes
  , withObject
  , resolving
  , toResolver
  , lift
  , SubEvent
  , GQLChannel(..)
  , ResponseEvent(..)
  , ResponseStream
  , resolve__typename
  , DataResolver(..)
  , FieldRes
  , WithOperation
  )
where

import           Control.Monad.Trans.Class      ( MonadTrans(..))
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import           Data.Text                      ( unpack
                                                , pack
                                                )
import           Control.Monad.Trans.Reader     (ReaderT(..), ask)

-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalResolvingError )
import           Data.Morpheus.Error.Selection  ( resolvingFailedError
                                                , subfieldsNotSelected
                                                )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( Selection(..)
                                                , SelectionContent(..)
                                                , ValidSelection
                                                , ValidSelectionRec
                                                , ValidSelectionSet
                                                , ValidSelection
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Message
                                                , Key
                                                , Name
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( MUTATION
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
                                                , cleanEvents
                                                , mapEvent
                                                , Event(..)
                                                , Channel(..)
                                                , StreamChannel
                                                , GQLChannel(..)
                                                , PushEvents(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( GQLValue(..)
                                                , ValidValue
                                                )
import           Data.Morpheus.Types.IO         ( renderResponse
                                                , GQLResponse
                                                )

class LiftOperation (o::OperationType) res where
  type ResError res :: *
  liftOperation :: Monad m => m (Either (ResError res) a) -> res o event m  a

type WithOperation (o :: OperationType) = LiftOperation o Resolver

type ResponseStream event m = ResultT (ResponseEvent m event) GQLError 'True m

data ResponseEvent m event
  = Publish event
  | Subscribe (SubEvent m event)

type SubEvent m event = Event (Channel event) (event -> m GQLResponse)

-- DataResolver
data DataResolver o e m =
    EnumRes  Name
  | UnionRes  (Name,[FieldRes o e m])
  | ObjectRes  [FieldRes o e m ]
  | UnionRef (FieldRes o e m)
  | InvalidRes Name

instance Semigroup (DataResolver o e m) where
  ObjectRes x <> ObjectRes y = ObjectRes (x <> y)
  _           <> _           = InvalidRes "can't merge: incompatible resolvers"

withObject
  :: (LiftOperation o Resolver, Monad m)
  => (ValidSelectionSet -> Resolver o e m value)
  -> (Key, ValidSelection)
  -> Resolver o e m value
withObject f (_, Selection { selectionContent = SelectionSet selection }) =
  f selection
withObject _ (key, Selection { selectionPosition }) =
  failure (subfieldsNotSelected key "" selectionPosition)

resolveObject
  :: (Monad m, LiftOperation o Resolver)
  => ValidSelectionSet
  -> DataResolver o e m
  -> Resolver o e m ValidValue
resolveObject selectionSet (ObjectRes resolvers) =
  gqlObject <$> traverse selectResolver selectionSet
 where
  selectResolver (key, selection@Selection { selectionAlias }) =
    (fromMaybe key selectionAlias, ) <$> lookupRes selection
   where
    lookupRes sel =
      (fromMaybe (const $ pure gqlNull) $ lookup key resolvers) (key, sel)
resolveObject _ _ =
  failure $ internalResolvingError "expected object as resolver"

resolveEnum
  :: (Monad m, LiftOperation o Resolver)
  => Name
  -> Name
  -> ValidSelectionRec
  -> Resolver o e m ValidValue
resolveEnum _        enum SelectionField              = pure $ gqlString enum
resolveEnum typeName enum (UnionSelection selections) = resolveObject
  currentSelection
  resolvers
 where
  enumObjectTypeName = typeName <> "EnumObject"
  currentSelection   = fromMaybe [] $ lookup enumObjectTypeName selections
  resolvers          = ObjectRes
    [ ("enum", const $ pure $ gqlString enum)
    , resolve__typename enumObjectTypeName
    ]
resolveEnum _ _ _ =
  failure $ internalResolvingError "wrong selection on enum value"

resolve__typename
  :: (Monad m, LiftOperation o Resolver)
  => Name
  -> (Key, (Key, ValidSelection) -> Resolver o e m ValidValue)
resolve__typename name = ("__typename", const $ pure $ gqlString name)

toResponseRes
  :: Monad m
  => Resolver o event m ValidValue
  -> (Key, ValidSelection)
  -> ResponseStream event m ValidValue
toResponseRes (QueryResolver resT) sel = cleanEvents $ (runReaderT $ runContextRes resT) sel
toResponseRes (MutResolver resT) sel = mapEvent Publish $ runReaderT ctx sel 
  where
    ctx = runContextRes $ do
      (events, value) <- resT
      pushEvents events
      return value
toResponseRes (SubResolver channels subRes) sel = ResultT $ pure $ Success
    { result   = gqlNull
    , warnings = []
    , events   = [Subscribe $ Event (map Channel channels) eventResolver]
    }
   where
    -- eventResolver :: event -> ResultT event m ValidValue
    eventResolver event = do 
      value <- runResultT $ runReaderT (runContextRes $ unQueryResolver (subRes event)) sel
      pure $ renderResponse value

newtype ContextRes event m a = ContextRes {
  runContextRes :: ReaderT (Name,ValidSelection) (ResultT event GQLError 'True m) a
} deriving (Functor, Applicative, Monad)

instance MonadTrans (ContextRes e) where
  lift = ContextRes . lift . lift

instance (Monad m) => Failure Message (ContextRes e m) where
  failure message = ContextRes $ do 
    selection <- ask
    lift $ failure [errorFromSelection selection message]

instance (Monad m) => PushEvents e (ContextRes e m) where
    pushEvents = ContextRes . lift . pushEvents 

errorFromSelection :: (Name,ValidSelection) -> Message -> GQLError
errorFromSelection (fieldName, Selection { selectionPosition })  = resolvingFailedError selectionPosition fieldName 

fromEitherSingle :: (Name,ValidSelection) ->  (Either String a) ->  Result ev GQLError co a
fromEitherSingle sel (Left  e)  = Failure [errorFromSelection sel (pack e)]
fromEitherSingle _ (Right a) = Success a [] []

--     
-- GraphQL Field Resolver
--
---------------------------------------------------------------
data Resolver (o::OperationType) event (m :: * -> * )  value where
    QueryResolver::{ unQueryResolver :: ContextRes () m value } -> Resolver QUERY   event m value
    MutResolver ::{ unMutResolver :: ContextRes event m ([event],value) } -> Resolver MUTATION event m  value
    SubResolver ::{
            subChannels :: [StreamChannel event] ,
            subResolver :: event -> Resolver QUERY event m value
        } -> Resolver SUBSCRIPTION event m  value

deriving instance (Functor m) => Functor (Resolver o e m)

-- Applicative
instance (LiftOperation o Resolver ,Monad m) => Applicative (Resolver o e m) where
  pure = liftOperation . pure . pure
  -------------------------------------
  (QueryResolver f) <*> (QueryResolver res) = QueryResolver $ f <*> res
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
  (QueryResolver f) >>= nextM = QueryResolver (f >>= unQueryResolver . nextM )

instance (Monad m) => Monad (Resolver MUTATION e m) where
  return = pure
  -----------------------------------------------------
  (MutResolver m1) >>= mFunc = MutResolver $ do
    (e1, v1) <- m1
    (e2, v2) <- unMutResolver $ mFunc v1
    pure (e1 <> e2, v2)

instance (MonadIO m) => MonadIO (Resolver QUERY e m) where
    liftIO = lift . liftIO
    
instance (MonadIO m) => MonadIO (Resolver MUTATION e m) where
    liftIO = lift . liftIO

-- Monad Transformers    
instance MonadTrans (Resolver QUERY e) where
  lift = liftOperation . fmap pure

instance MonadTrans (Resolver MUTATION e) where
  lift = liftOperation . fmap pure

-- LiftOperation
instance LiftOperation QUERY Resolver where
  type ResError Resolver = String
  liftOperation res = QueryResolver $ ContextRes $ do 
    selection <- ask
    lift $ ResultT $ fmap (fromEitherSingle selection) res

instance LiftOperation MUTATION Resolver where
  type ResError Resolver = String
  liftOperation res = MutResolver $ ContextRes $ do 
    selection <- ask
    lift $ ResultT $ (fromEitherSingle selection) <$> (fmap (fmap ([], )) res)

instance LiftOperation SUBSCRIPTION Resolver where
  type ResError Resolver = String
  liftOperation = SubResolver [] . const . liftOperation

-- Failure
instance (LiftOperation o Resolver, Monad m) => Failure Message (Resolver o e m) where
  failure = liftOperation . pure . Left . unpack

instance (LiftOperation o Resolver, Monad m) => Failure GQLErrors (Resolver o e m) where
-- failure = liftOperation . pure . Left . unpack

-- Type Helpers  
type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (Resolver SUBSCRIPTION m e) = Resolver QUERY m e

-- RESOLVING
type FieldRes o e m
  = (Key, (Key, ValidSelection) -> Resolver o e m ValidValue)

toResolver
  :: (LiftOperation o Resolver, Monad m)
  => Validation a
  -> (a -> Resolver o e m b)
  -> Resolver o e m b
toResolver Success { result } f = f result
toResolver (Failure errors) _ = failure errors

updateContext :: ContextRes e m a -> (Name,ValidSelection) ->  ContextRes e m a
updateContext res = ContextRes . ReaderT . const . (runReaderT $ runContextRes res)

resolving
  :: forall o e m value
   . Monad m
  => (value -> (Key, ValidSelection) -> Resolver o e m ValidValue)
  -> Resolver o e m value
  -> (Key, ValidSelection)
  -> Resolver o e m ValidValue 
resolving encode (QueryResolver res) selection = QueryResolver $ do 
    value <- updateContext res selection
    unQueryResolver $ encode value selection
resolving encode (MutResolver res) selection = MutResolver $ do
    (events, value) <- updateContext res selection
    pushEvents events
    unMutResolver $ encode value selection
resolving encode (SubResolver subChannels res) selection = do 
    SubResolver {
      subChannels,
      subResolver = \events -> do
        value <- QueryResolver $ updateContext (unQueryResolver $ res events) selection
        (subResolver $ encode value selection)  events
    }

-- map Resolving strategies 
class MapStrategy (from :: OperationType) (to :: OperationType) where
   mapStrategy :: Monad m => Resolver from e m a -> Resolver to e m a

instance MapStrategy o o where
  mapStrategy = id

instance MapStrategy QUERY SUBSCRIPTION where
  mapStrategy res = SubResolver [] (const res)

-------------------------------------------------------------------
-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you can use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: query (Resolver QUERY event m)
  , mutationResolver     :: mut (Resolver MUTATION event m)
  , subscriptionResolver :: sub (Resolver SUBSCRIPTION  event m)
  }