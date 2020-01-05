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
  , runDataResolver
  , toResponseRes
  , withObject
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
  :: (LiftOperation o, Monad m)
  => (ValidSelectionSet -> Resolver o e m value)
  -> (Key, ValidSelection)
  -> Resolver o e m value
withObject f (_, Selection { selectionContent = SelectionSet selection }) =
  f selection
withObject _ (key, Selection { selectionPosition }) =
  failure (subfieldsNotSelected key "" selectionPosition)

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

resolve__typename
  :: (Monad m, LiftOperation o)
  => Name
  -> (Key, Resolver o e m ValidValue)
resolve__typename name = ("__typename", pure $ gqlString name)

toResponseRes
  :: Monad m
  => Resolver o event m ValidValue
  -> (Key, ValidSelection)
  -> ResponseStream event m ValidValue
toResponseRes (QueryResolver resT) sel = cleanEvents $ (runReaderT $ runContextRes resT) sel
toResponseRes (MutResolver resT) sel = mapEvent Publish $ (runReaderT $ runContextRes $ resT) sel 
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

getContext :: (Monad m) => ContextRes e m (Name,ValidSelection)
getContext = ContextRes $ ask 

newtype ContextRes event m a = ContextRes {
  runContextRes :: ReaderT (Name,ValidSelection) (ResultT event GQLError 'True m) a
} deriving (Functor, Applicative, Monad)

instance Monad m => MonadFail (ContextRes event m) where 
  fail = failure . pack

instance MonadTrans (ContextRes e) where
  lift = ContextRes . lift . lift

instance (Monad m) => Failure Message (ContextRes e m) where
  failure message = ContextRes $ do 
    selection <- ask
    lift $ failure [errorFromSelection selection message]

instance (Monad m) => Failure GQLErrors (ContextRes e m) where
  failure = ContextRes . lift . failure 

instance (Monad m) => PushEvents e (ContextRes e m) where
    pushEvents = ContextRes . lift . pushEvents 

errorFromSelection :: (Name,ValidSelection) -> Message -> GQLError
errorFromSelection (fieldName, Selection { selectionPosition })  = resolvingFailedError selectionPosition fieldName 

--     
-- GraphQL Field Resolver
--
---------------------------------------------------------------
data Resolver (o::OperationType) event (m :: * -> * )  value where
    QueryResolver::{ unQueryResolver :: ContextRes () m value } -> Resolver QUERY   event m value
    MutResolver ::{ unMutResolver :: ContextRes event m value } -> Resolver MUTATION event m  value
    SubResolver ::{
            subChannels :: [StreamChannel event] ,
            subResolver :: event -> Resolver QUERY event m value
        } -> Resolver SUBSCRIPTION event m  value

deriving instance (Functor m) => Functor (Resolver o e m)

-- Applicative
instance (LiftOperation o ,Monad m) => Applicative (Resolver o e m) where
  pure = packResolver . pure
  QueryResolver res1 <*> QueryResolver res2 = QueryResolver $ res1 <*> res2
  MutResolver res1 <*> MutResolver res2 = MutResolver $ res1 <*> res2
  SubResolver e1 f <*> SubResolver e2 res = SubResolver (e1 <> e2) subRes
    where subRes event = f event <*> res event

-- Monad 
instance (Monad m) => Monad (Resolver QUERY e m) where
  return = pure
  (>>=) = unsafeBind

instance (Monad m) => Monad (Resolver MUTATION e m) where
  return = pure
  (>>=) = unsafeBind

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
unsafeBind (QueryResolver x) m2 = QueryResolver (x >>= unQueryResolver . m2)
unsafeBind (MutResolver x) m2 = MutResolver (x >>= unMutResolver . m2)
unsafeBind (SubResolver subChannels res) m2 = do 
     SubResolver {
       subChannels,
       subResolver = \events -> do
         value <- res events
         -- this Channels will be ignored
         (subResolver $ m2 value) events
     }

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

class LiftOperation (o::OperationType) where
  packResolver :: Monad m => ContextRes e m a -> Resolver o e m a
  withResolver :: Monad m => ContextRes e m a -> (a -> Resolver o e m b) -> Resolver o e m b
  setSelection :: (Name, ValidSelection) -> Resolver o e m a -> Resolver o e m a 

clearCTXEvents :: (Functor m) => ContextRes e1 m a -> ContextRes e2 m a
clearCTXEvents (ContextRes (ReaderT x)) = ContextRes $ ReaderT $ \sel -> cleanEvents (x sel)

-- packResolver
instance LiftOperation QUERY where
  packResolver = QueryResolver . clearCTXEvents
  withResolver ctxRes toRes = QueryResolver $ do 
     v <- clearCTXEvents ctxRes 
     unQueryResolver $ toRes v
  setSelection sel (QueryResolver res)  = QueryResolver (updateContext res sel) 

updateContext :: ContextRes e m a -> (Name,ValidSelection) ->  ContextRes e m a
updateContext res = ContextRes . ReaderT . const . (runReaderT $ runContextRes res)

instance LiftOperation MUTATION where
  packResolver = MutResolver
  withResolver ctxRes toRes = MutResolver $ ctxRes >>= unMutResolver . toRes 
  setSelection sel (MutResolver res)  = MutResolver (updateContext res sel) 

instance LiftOperation SUBSCRIPTION where
  packResolver = SubResolver [] . const . packResolver
  -- TODO: FIXME: this real events instead of []
  withResolver ctxRes toRes = SubResolver [] $ \ev -> QueryResolver $ do 
     v <- clearCTXEvents ctxRes
     unQueryResolver $ (subResolver $ toRes v) ev 
  
  setSelection sel (SubResolver events res)  = SubResolver events $ \e -> QueryResolver $ do 
      updateContext (unQueryResolver (res e)) sel


instance (Monad m) => PushEvents e (Resolver MUTATION e m)  where
    pushEvents = packResolver . pushEvents 

-- Type Helpers  
type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (Resolver SUBSCRIPTION m e) = Resolver QUERY m e

-- RESOLVING
type FieldRes o e m
  = (Key, Resolver o e m ValidValue)

toResolver
  :: forall o e m a b. (LiftOperation o, Monad m)
  => (ValidArguments -> Validation a)
  -> (a -> Resolver o e m b)
  -> Resolver o e m b
toResolver toArgs  = withResolver args 
 where 
  args :: ContextRes e m a
  args = do
    (_,Selection { selectionArguments }) <- getContext
    let resT = ResultT $ pure $ toArgs selectionArguments
    ContextRes $ lift $ cleanEvents resT


pickSelection :: Name -> [(Name, ValidSelectionSet)] -> ValidSelectionSet
pickSelection name = fromMaybe [] . lookup name

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