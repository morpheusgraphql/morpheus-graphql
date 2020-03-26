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
  , SubEvent
  , GQLChannel(..)
  , ResponseEvent(..)
  , ResponseStream
  , resolve__typename
  , DataResolver(..)
  , FieldRes
  , WithOperation
  , subscribe
  , Context(..)
  , unsafeInternalContext
  )
where

import           Control.Monad.Fail             (MonadFail(..))
import           Control.Monad.Trans.Class      ( MonadTrans(..))
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )
import           Control.Monad.Trans.Reader     (ReaderT(..), ask,mapReaderT, withReaderT)
import           Data.Text                      (pack)

-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalResolvingError )
import           Data.Morpheus.Error.Selection  ( subfieldsNotSelected )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( Selection(..)
                                                , SelectionContent(..)
                                                , ValidSelection
                                                , ValidSelectionRec
                                                , ValidSelectionSet
                                                , ValidSelection
                                                , ValidArguments
                                                , ValidOperation
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Message
                                                , Key
                                                , Name
                                                , OperationType
                                                , QUERY
                                                , MUTATION
                                                , SUBSCRIPTION
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( Schema
                                                )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( GQLErrors
                                                , GQLError(..)
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

data Context = Context {
  currentSelection :: (Name,ValidSelection),
  schema :: Schema,
  operation :: ValidOperation
} deriving (Show)

-- Resolver Internal State
newtype ResolverState event m a = ResolverState {
  runResolverState :: ReaderT Context (ResultT event GQLError 'True m) a
} deriving (Functor, Applicative, Monad)

instance Monad m => MonadFail (ResolverState event m) where 
  fail = failure . pack

instance MonadTrans (ResolverState e) where
  lift = ResolverState . lift . lift

instance (Monad m) => Failure Message (ResolverState e m) where
  failure message = ResolverState $ do 
    selection <- currentSelection <$> ask 
    lift $ failure [resolverFailureMessage selection message]

instance (Monad m) => Failure GQLErrors (ResolverState e m) where
  failure = ResolverState . lift . failure 

instance (Monad m) => PushEvents e (ResolverState e m) where
    pushEvents = ResolverState . lift . pushEvents 


mapResolverState :: 
  ( ReaderT Context (ResultT e1 GQLError 'True m1) a1 
    -> ReaderT Context (ResultT e2 GQLError 'True m2) a2 
  ) -> ResolverState e1 m1 a1 
    -> ResolverState e2 m2 a2
mapResolverState f (ResolverState x) = ResolverState (f x)


getState :: (Monad m) => ResolverState e m (Name,ValidSelection)
getState = ResolverState $ currentSelection <$> ask 

setState :: (Name,ValidSelection) -> ResolverState e m a -> ResolverState e m a
setState currentSelection = mapResolverState (withReaderT (\ctx -> ctx { currentSelection } ))

-- clear evets and starts new resolver with diferenct type of events but with same value
-- use properly. only if you know what you are doing
clearStateResolverEvents :: (Functor m) => ResolverState e1 m a -> ResolverState e2 m a
clearStateResolverEvents = mapResolverState (mapReaderT cleanEvents)

resolverFailureMessage :: (Name,ValidSelection) -> Message -> GQLError
resolverFailureMessage (name, Selection { selectionPosition }) message = GQLError
  { message   = "Failure on Resolving Field \"" <> name <> "\": " <> message
  , locations = [selectionPosition]
  }

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

-- MonadIO
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

-- PushEvents
instance (Monad m) => PushEvents e (Resolver MUTATION e m)  where
    pushEvents = packResolver . pushEvents 

class LiftOperation (o::OperationType) where
  packResolver :: Monad m => ResolverState e m a -> Resolver o e m a
  withResolver :: Monad m => ResolverState e m a -> (a -> Resolver o e m b) -> Resolver o e m b

-- packResolver
instance LiftOperation QUERY where
  packResolver = ResolverQ . clearStateResolverEvents
  withResolver ctxRes toRes = ResolverQ $ do 
     v <- clearStateResolverEvents ctxRes 
     runResolverQ $ toRes v

instance LiftOperation MUTATION where
  packResolver = ResolverM
  withResolver ctxRes toRes = ResolverM $ ctxRes >>= runResolverM . toRes 

instance LiftOperation SUBSCRIPTION where
  packResolver = ResolverS . pure . lift . packResolver
  withResolver ctxRes toRes = ResolverS $ do 
    value <- clearStateResolverEvents ctxRes
    runResolverS $ toRes value

setSelection :: Monad m => (Name, ValidSelection) -> Resolver o e m a -> Resolver o e m a 
setSelection sel (ResolverQ res)  = ResolverQ (setState sel res)
setSelection sel (ResolverM res)  = ResolverM (setState sel res) 
setSelection sel (ResolverS resM)  = ResolverS $ do
    res <- resM
    pure $ ReaderT $ \e -> ResolverQ $ setState sel (runResolverQ (runReaderT res e)) 

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
         let (resA :: Resolver QUERY e m a) = runReaderT readResA e
         (valA :: a) <- runResolverQ resA
         (readResB :: ReaderT e (Resolver QUERY e m) b) <- clearStateResolverEvents $ runResolverS (m2 valA) 
         runResolverQ $ runReaderT readResB e

subscribe :: forall e m a . (PushEvents (Channel e) (ResolverState (Channel e) m), Monad m) => [StreamChannel e] -> Resolver QUERY e m (e -> Resolver QUERY e m a) -> Resolver SUBSCRIPTION e m a
subscribe ch res = ResolverS $ do 
  pushEvents (map Channel ch :: [Channel e])
  (eventRes :: e -> Resolver QUERY e m a) <- clearStateResolverEvents (runResolverQ res)
  pure $ ReaderT eventRes

unsafeInternalContext :: (Monad m, LiftOperation o) => Resolver o e m Context
unsafeInternalContext = packResolver $ ResolverState ask 

-- Converts Subscription Resolver Type to Query Resolver
type family UnSubResolver (a :: * -> * ) :: (* -> *)
type instance UnSubResolver (Resolver SUBSCRIPTION e m) = Resolver QUERY e m

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
    (_,Selection { selectionArguments }) <- getState
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

outputSelectionName :: (Name,ValidSelection) -> Name
outputSelectionName (name,Selection { selectionAlias }) = fromMaybe name selectionAlias

resolveObject
  :: forall o e m. (LiftOperation o , Monad m)
  => ValidSelectionSet
  -> DataResolver o e m
  -> Resolver o e m ValidValue
resolveObject selectionSet (ObjectRes resolvers) =
  gqlObject <$> traverse resolver selectionSet
 where
  resolver :: (Name,ValidSelection) -> Resolver o e m (Name,ValidValue)
  resolver sel@(name,_) = setSelection sel $ (outputSelectionName sel, ) <$> lookupRes name resolvers
resolveObject _ _ =
  failure $ internalResolvingError "expected object as resolver"

toEventResolver :: Monad m => ReaderT event (Resolver QUERY event m) ValidValue -> Context -> event -> m GQLResponse
toEventResolver (ReaderT subRes) sel event = do 
  value <- runResultT $ runReaderT (runResolverState $ runResolverQ (subRes event)) sel
  pure $ renderResponse value

runDataResolver :: (Monad m, LiftOperation o) => Name -> DataResolver o e m -> Resolver o e m ValidValue
runDataResolver typename  = withResolver getState . __encode
   where
    __encode obj (key, sel@Selection { selectionContent })  = encodeNode obj selectionContent 
      where 
      -- Object -----------------
      encodeNode (ObjectRes fields) _ = withObject encodeObject (key, sel)
        where
        encodeObject selection =
          resolveObject selection
            $ ObjectRes
            $ resolve__typename typename
            : fields
      encodeNode (EnumRes enum) _ =
        resolveEnum typename enum selectionContent
      -- Type Reference --------
      encodeNode (UnionRef (fieldTypeName, fieldResolver)) (UnionSelection selections)
        = setSelection (key, sel { selectionContent = SelectionSet currentSelection }) fieldResolver
          where currentSelection = pickSelection fieldTypeName selections
      -- Union Record ----------------
      encodeNode (UnionRes (name, fields)) (UnionSelection selections) =
        resolveObject selection resolver
        where
          selection = pickSelection name selections
          resolver = ObjectRes (resolve__typename name : fields)
      encodeNode _ _ = failure $ internalResolvingError
        "union Resolver should only recieve UnionSelection"

runResolver
  :: Monad m
  => Resolver o event m ValidValue
  -> Context
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