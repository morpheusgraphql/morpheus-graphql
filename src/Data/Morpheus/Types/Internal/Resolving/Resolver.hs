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
  , Resolver
  , MapStrategy(..)
  , LiftOperation
  , resolveObject
  , runDataResolver
  , runResolver
  , unsafeBind
  , toResolver
  , lift
  , getContext
  , SubEvent
  , GQLChannel(..)
  , ResponseEvent(..)
  , ResponseStream
  , resolve__typename
  , DataResolver(..)
  , FieldRes
  , WithOperation
  , subscribe
  )
where

import           Control.Monad.Fail             (MonadFail(..))
import           Control.Monad.Trans.Class      ( MonadTrans(..))
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import           Control.Monad.Trans.Reader     (ReaderT(..), ask)
import           Data.Text                      (pack)

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
                                                , ValidArguments
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

type WithOperation (o :: OperationType) = LiftOperation o

type ResponseStream event m = ResultT (ResponseEvent m event) GQLError 'True m

data ResponseEvent m event
  = Publish event
  | Subscribe (SubEvent m event)

type SubEvent m event = Event (Channel event) (event -> m GQLResponse)

-- Resolver Internal State
newtype ResolverState event m a = ResolverState {
  runResolverState :: ReaderT (Name,ValidSelection) (ResultT event GQLError 'True m) a
} deriving (Functor, Applicative, Monad)

instance Monad m => MonadFail (ResolverState event m) where 
  fail = failure . pack

instance MonadTrans (ResolverState e) where
  lift = ResolverState . lift . lift

instance (Monad m) => Failure Message (ResolverState e m) where
  failure message = ResolverState $ do 
    selection <- ask
    lift $ failure [errorFromSelection selection message]

instance (Monad m) => Failure GQLErrors (ResolverState e m) where
  failure = ResolverState . lift . failure 

instance (Monad m) => PushEvents e (ResolverState e m) where
    pushEvents = ResolverState . lift . pushEvents 

errorFromSelection :: (Name,ValidSelection) -> Message -> GQLError
errorFromSelection (fieldName, Selection { selectionPosition })  = resolvingFailedError selectionPosition fieldName 

getContext :: (Monad m) => ResolverState e m (Name,ValidSelection)
getContext = ResolverState $ ask 

updateContext :: ResolverState e m a -> (Name,ValidSelection) ->  ResolverState e m a
updateContext res = ResolverState . ReaderT . const . (runReaderT $ runResolverState res)

clearCTXEvents :: (Functor m) => ResolverState e1 m a -> ResolverState e2 m a
clearCTXEvents (ResolverState (ReaderT x)) = ResolverState $ ReaderT $ \sel -> cleanEvents (x sel)


--     
-- GraphQL Field Resolver
--
---------------------------------------------------------------
data Resolver (o::OperationType) event (m :: * -> * )  value where
    ResolverQ :: { runResolverQ :: ResolverState () m value } -> Resolver QUERY event m value
    ResolverM :: { runResolverM :: ResolverState event m value } -> Resolver MUTATION event m  value
    ResolverS :: { runResolverS :: ResolverState (Channel event) m (ReaderT event (Resolver QUERY event m) value) } -> Resolver SUBSCRIPTION event m  value

deriving instance (Functor m) => Functor (Resolver o e m)

-- Applicative
instance (LiftOperation o ,Monad m) => Applicative (Resolver o e m) where
  pure = packResolver . pure
  ResolverQ r1 <*> ResolverQ r2 = ResolverQ $ r1 <*> r2
  ResolverM r1 <*> ResolverM r2 = ResolverM $ r1 <*> r2
  ResolverS r1 <*> ResolverS r2 = ResolverS $ (<*>) <$> r1 <*> r2

-- Monad 
instance (Monad m) => Monad (Resolver QUERY e m) where
  return = pure
  (>>=) = unsafeBind

instance (Monad m) => Monad (Resolver MUTATION e m) where
  return = pure
  (>>=) = unsafeBind

instance (MonadIO m) => MonadIO (Resolver QUERY e m) where
    liftIO = lift . liftIO
    
instance (MonadIO m) => MonadIO (Resolver MUTATION e m) where
    liftIO = lift . liftIO

-- Monad Transformers    
instance MonadTrans (Resolver QUERY e) where
  lift = packResolver . lift

instance MonadTrans (Resolver MUTATION e) where
  lift = packResolver . lift

-- Failure
instance (LiftOperation o, Monad m) => Failure Message (Resolver o e m) where
   failure = packResolver .failure

instance (LiftOperation o, Monad m) => Failure GQLErrors (Resolver o e m) where
  failure = packResolver . failure 

instance (Monad m) => PushEvents e (Resolver MUTATION e m)  where
    pushEvents = packResolver . pushEvents 

class LiftOperation (o::OperationType) where
  packResolver :: Monad m => ResolverState e m a -> Resolver o e m a
  withResolver :: Monad m => ResolverState e m a -> (a -> Resolver o e m b) -> Resolver o e m b
  setSelection :: Monad m => (Name, ValidSelection) -> Resolver o e m a -> Resolver o e m a 

-- packResolver
instance LiftOperation QUERY where
  packResolver = ResolverQ . clearCTXEvents
  withResolver ctxRes toRes = ResolverQ $ do 
     v <- clearCTXEvents ctxRes 
     runResolverQ $ toRes v
  setSelection sel (ResolverQ res)  = ResolverQ (updateContext res sel) 

instance LiftOperation MUTATION where
  packResolver = ResolverM
  withResolver ctxRes toRes = ResolverM $ ctxRes >>= runResolverM . toRes 
  setSelection sel (ResolverM res)  = ResolverM (updateContext res sel) 

instance LiftOperation SUBSCRIPTION where
  packResolver = ResolverS . pure . lift . packResolver
  withResolver ctxRes toRes = ResolverS $ do 
    value <- clearCTXEvents ctxRes
    runResolverS $ toRes value
  setSelection sel (ResolverS resM)  = ResolverS $ do 
    res <- resM
    pure $ ReaderT $ \e -> ResolverQ $ updateContext (runResolverQ (runReaderT res e)) sel

-- unsafe variant of >>= , not for public api. user can be confused: 
--  ignores `channels` on second Subsciption, only returns events from first Subscription monad.
--    reason: second monad is waiting for `event` until he does not have some event can't tell which 
--            channel does it want to listen
unsafeBind
  :: forall o e m a b
   . Monad m
  =>  Resolver o e m a
  -> (a -> Resolver o e m b)
  -> Resolver o e m b 
unsafeBind (ResolverQ x) m2 = ResolverQ (x >>= runResolverQ . m2)
unsafeBind (ResolverM x) m2 = ResolverM (x >>= runResolverM . m2)
unsafeBind (ResolverS res) m2 = ResolverS $ do 
    (readResA :: ReaderT e (Resolver QUERY e m) a ) <- res 
    pure $ ReaderT $ \e -> ResolverQ $ do 
         let (resA :: Resolver QUERY e m a) = (runReaderT $ readResA) e
         (valA :: a) <- runResolverQ resA
         (readResB :: ReaderT e (Resolver QUERY e m) b) <- clearCTXEvents $ runResolverS (m2 valA) 
         runResolverQ $ runReaderT readResB e

subscribe :: forall e m a . (PushEvents (Channel e) (ResolverState (Channel e) m), Monad m) => [StreamChannel e] -> Resolver QUERY e m (e -> Resolver QUERY e m a) -> Resolver SUBSCRIPTION e m a
subscribe ch res = ResolverS $ do 
  pushEvents (map Channel ch :: [Channel e])
  (eventRes :: e -> Resolver QUERY e m a) <- clearCTXEvents (runResolverQ res)
  pure $ ReaderT eventRes

-- Type Helpers  
type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (Resolver SUBSCRIPTION m e) = Resolver QUERY m e

-- map Resolving strategies 
class MapStrategy (from :: OperationType) (to :: OperationType) where
   mapStrategy :: Monad m => Resolver from e m a -> Resolver to e m a

instance MapStrategy o o where
  mapStrategy = id

instance MapStrategy QUERY SUBSCRIPTION where
  mapStrategy  = ResolverS . pure . lift

--
-- Selection Processing
--
type FieldRes o e m
  = (Key, Resolver o e m ValidValue)

toResolver
  :: forall o e m a b. (LiftOperation o, Monad m)
  => (ValidArguments -> Validation a)
  -> (a -> Resolver o e m b)
  -> Resolver o e m b
toResolver toArgs  = withResolver args 
 where 
  args :: ResolverState e m a
  args = do
    (_,Selection { selectionArguments }) <- getContext
    let resT = ResultT $ pure $ toArgs selectionArguments
    ResolverState $ lift $ cleanEvents resT

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

pickSelection :: Name -> [(Name, ValidSelectionSet)] -> ValidSelectionSet
pickSelection name = fromMaybe [] . lookup name

resolve__typename
  :: (Monad m, LiftOperation o)
  => Name
  -> (Key, Resolver o e m ValidValue)
resolve__typename name = ("__typename", pure $ gqlString name)

resolveEnum
  :: (Monad m, LiftOperation o)
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
    [ ("enum", pure $ gqlString enum)
    , resolve__typename enumObjectTypeName
    ]
resolveEnum _ _ _ =
  failure $ internalResolvingError "wrong selection on enum value"

withObject
  :: (LiftOperation o, Monad m)
  => (ValidSelectionSet -> Resolver o e m value)
  -> (Key, ValidSelection)
  -> Resolver o e m value
withObject f (key, Selection { selectionContent , selectionPosition }) = checkContent selectionContent
 where
  checkContent (SelectionSet selection) = f selection
  checkContent _ = failure (subfieldsNotSelected key "" selectionPosition)


lookupRes :: (LiftOperation o, Monad m) => Name -> [(Name,Resolver o e m ValidValue)] -> Resolver o e m ValidValue
lookupRes key = fromMaybe (pure gqlNull) . lookup key 

resolveObject
  :: forall o e m. (LiftOperation o , Monad m)
  => ValidSelectionSet
  -> DataResolver o e m
  -> Resolver o e m ValidValue
resolveObject selectionSet (ObjectRes resolvers) =
  gqlObject <$> traverse selectResolver selectionSet
 where
  selectResolver :: (Name,ValidSelection) -> Resolver o e m (Name,ValidValue)
  selectResolver sel@(key,Selection { selectionAlias }) = setSelection sel $ do 
    let selName = fromMaybe key selectionAlias
    (selName, ) <$> lookupRes key resolvers
resolveObject _ _ =
  failure $ internalResolvingError "expected object as resolver"

toEventResolver :: Monad m => (ReaderT event (Resolver QUERY event m) ValidValue) -> (Name,ValidSelection) -> event -> m GQLResponse
toEventResolver (ReaderT subRes) sel event = do 
  value <- runResultT $ runReaderT (runResolverState $ runResolverQ (subRes event)) sel
  pure $ renderResponse value

runDataResolver :: (Monad m, LiftOperation o) => Name -> DataResolver o e m -> Resolver o e m ValidValue
runDataResolver typename resolver = withResolver getContext (__encode resolver)
   where
    __encode obj (key, sel@Selection { selectionContent })  = encodeNode obj selectionContent 
      where 
      encodeNode (ObjectRes fields) _ = withObject encodeObject (key, sel)
        where
        encodeObject selection =
          resolveObject selection
            $ ObjectRes
            $ resolve__typename typename
            : fields
      encodeNode (EnumRes enum) _ =
        resolveEnum typename enum selectionContent
      -- Type References --------------------------------------------------------------
      encodeNode (UnionRef (fieldTypeName, fieldResolver)) (UnionSelection selections)
        = setSelection (key, sel { selectionContent = SelectionSet currentSelection }) fieldResolver
          where currentSelection = pickSelection fieldTypeName selections
      -- RECORDS ----------------------------------------------------------------------------
      encodeNode (UnionRes (name, fields)) (UnionSelection selections) =
        resolveObject selection resolvers
        where
          selection = pickSelection name selections
          resolvers = ObjectRes (resolve__typename name : fields)
      encodeNode _ _ = failure $ internalResolvingError
        "union Resolver should only recieve UnionSelection"

runResolver
  :: Monad m
  => Resolver o event m ValidValue
  -> (Key, ValidSelection)
  -> ResponseStream event m ValidValue
runResolver (ResolverQ resT) sel = cleanEvents $ (runReaderT $ runResolverState resT) sel
runResolver (ResolverM resT) sel = mapEvent Publish $ (runReaderT $ runResolverState $ resT) sel 
runResolver (ResolverS resT) sel = ResultT $ do 
    (readResValue :: Result (Channel event1) GQLError 'True (ReaderT event (Resolver QUERY event m) ValidValue))  <- runResultT $ (runReaderT $ runResolverState $ resT) sel
    pure $ case readResValue of 
      Failure x -> Failure x
      Success { warnings ,result , events = channels } -> do
        let eventRes = toEventResolver result sel
        Success {
          events = [Subscribe $ Event channels eventRes],
          warnings,
          result = gqlNull
        } 

-------------------------------------------------------------------
-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you can use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: query (Resolver QUERY event m)
  , mutationResolver     :: mut (Resolver MUTATION event m)
  , subscriptionResolver :: sub (Resolver SUBSCRIPTION  event m)
  }