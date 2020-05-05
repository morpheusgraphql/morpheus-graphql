{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Types.Internal.Resolving.Resolver
  ( Event (..),
    GQLRootResolver (..),
    UnSubResolver,
    Resolver,
    MapStrategy (..),
    LiftOperation,
    unsafeBind,
    toResolver,
    lift,
    subscribe,
    SubEvent,
    GQLChannel (..),
    ResponseEvent (..),
    ResponseStream,
    ObjectDeriving (..),
    Deriving (..),
    WithOperation,
    Context (..),
    unsafeInternalContext,
    runResolverModel,
    setTypeName,
    ResolverModel (..),
    FieldDeriving (..),
    liftStateless,
  )
where

import Control.Monad.Fail (MonadFail (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT, withReaderT)
-- MORPHEUS
import Data.Morpheus.Error.Internal (internalResolvingError)
import Data.Morpheus.Error.Selection (subfieldsNotSelected)
import Data.Morpheus.Types.IO
  ( GQLResponse,
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLError (..),
    GQLErrors,
    MUTATION,
    Message,
    Name,
    OperationType,
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.Data
  ( Arguments,
    Schema,
  )
import Data.Morpheus.Types.Internal.AST.MergeSet
  ( toOrderedMap,
  )
import Data.Morpheus.Types.Internal.AST.Selection
  ( Operation (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    UnionSelection,
    UnionTag (..),
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( GQLValue (..),
    ObjectEntry (..),
    ScalarValue (..),
    ValidValue,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Operation
  ( KeyOf (..),
    Merge (..),
    empty,
    keyOf,
    selectOr,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Channel (..),
    Event (..),
    Eventless,
    Failure (..),
    GQLChannel (..),
    PushEvents (..),
    Result (..),
    ResultT (..),
    StreamChannel,
    cleanEvents,
    mapEvent,
    statelessToResultT,
  )
import Data.Semigroup
  ( (<>),
    Semigroup (..),
  )
import Data.Text (pack)

type WithOperation (o :: OperationType) = LiftOperation o

type ResponseStream event (m :: * -> *) = ResultT (ResponseEvent event m) m

data ResponseEvent event (m :: * -> *)
  = Publish event
  | Subscribe (SubEvent event m)

type SubEvent event m = Event (Channel event) (event -> m GQLResponse)

data Context = Context
  { currentSelection :: Selection VALID,
    schema :: Schema,
    operation :: Operation VALID,
    currentTypeName :: Name
  }
  deriving (Show)

-- Resolver Internal State
newtype ResolverState event m a = ResolverState
  { runResolverState :: ReaderT Context (ResultT event m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad
    )

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
  ( ReaderT Context (ResultT e m) a ->
    ReaderT Context (ResultT e' m') a'
  ) ->
  ResolverState e m a ->
  ResolverState e' m' a'
mapResolverState f (ResolverState x) = ResolverState (f x)

getState :: (Monad m) => ResolverState e m (Selection VALID)
getState = ResolverState $ currentSelection <$> ask

mapState :: (Context -> Context) -> ResolverState e m a -> ResolverState e m a
mapState f = mapResolverState (withReaderT f)

-- clear evets and starts new resolver with diferenct type of events but with same value
-- use properly. only if you know what you are doing
clearStateResolverEvents :: (Functor m) => ResolverState e m a -> ResolverState e' m a
clearStateResolverEvents = mapResolverState (mapReaderT cleanEvents)

resolverFailureMessage :: Selection VALID -> Message -> GQLError
resolverFailureMessage Selection {selectionName, selectionPosition} message =
  GQLError
    { message = "Failure on Resolving Field \"" <> selectionName <> "\": " <> message,
      locations = [selectionPosition]
    }

--
-- GraphQL Field Resolver
--
---------------------------------------------------------------
data Resolver (o :: OperationType) event (m :: * -> *) value where
  ResolverQ :: {runResolverQ :: ResolverState () m value} -> Resolver QUERY event m value
  ResolverM :: {runResolverM :: ResolverState event m value} -> Resolver MUTATION event m value
  ResolverS :: {runResolverS :: ResolverState (Channel event) m (ReaderT event (Resolver QUERY event m) value)} -> Resolver SUBSCRIPTION event m value

instance Show (Resolver o e m value) where
  show ResolverQ {} = "Resolver QUERY e m a"
  show ResolverM {} = "Resolver MUTATION e m a"
  show ResolverS {} = "Resolver SUBSCRIPTION e m a"

deriving instance (Functor m) => Functor (Resolver o e m)

-- Applicative
instance (LiftOperation o, Monad m) => Applicative (Resolver o e m) where
  pure = packResolver . pure
  ResolverQ r1 <*> ResolverQ r2 = ResolverQ $ r1 <*> r2
  ResolverM r1 <*> ResolverM r2 = ResolverM $ r1 <*> r2
  ResolverS r1 <*> ResolverS r2 = ResolverS $ (<*>) <$> r1 <*> r2

-- Monad
instance (Monad m, LiftOperation o) => Monad (Resolver o e m) where
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
  failure = packResolver . failure

instance (LiftOperation o, Monad m) => Failure GQLErrors (Resolver o e m) where
  failure = packResolver . failure

-- PushEvents
instance (Monad m) => PushEvents e (Resolver MUTATION e m) where
  pushEvents = packResolver . pushEvents

liftStateless ::
  ( LiftOperation o,
    Monad m
  ) =>
  Eventless a ->
  Resolver o e m a
liftStateless =
  packResolver
    . ResolverState
    . ReaderT
    . const
    . statelessToResultT

class LiftOperation (o :: OperationType) where
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

mapResolverContext :: Monad m => (Context -> Context) -> Resolver o e m a -> Resolver o e m a
mapResolverContext f (ResolverQ res) = ResolverQ (mapState f res)
mapResolverContext f (ResolverM res) = ResolverM (mapState f res)
mapResolverContext f (ResolverS resM) = ResolverS $ do
  res <- resM
  pure $ ReaderT $ \e -> ResolverQ $ mapState f (runResolverQ (runReaderT res e))

setSelection :: Monad m => Selection VALID -> Resolver o e m a -> Resolver o e m a
setSelection currentSelection =
  mapResolverContext (\ctx -> ctx {currentSelection})

setTypeName :: Monad m => Name -> Resolver o e m a -> Resolver o e m a
setTypeName currentTypeName =
  mapResolverContext (\ctx -> ctx {currentTypeName})

-- unsafe variant of >>= , not for public api. user can be confused:
--  ignores `channels` on second Subsciption, only returns events from first Subscription monad.
--    reason: second monad is waiting for `event` until he does not have some event can't tell which
--            channel does it want to listen
unsafeBind ::
  forall o e m a b.
  Monad m =>
  Resolver o e m a ->
  (a -> Resolver o e m b) ->
  Resolver o e m b
unsafeBind (ResolverQ x) m2 = ResolverQ (x >>= runResolverQ . m2)
unsafeBind (ResolverM x) m2 = ResolverM (x >>= runResolverM . m2)
unsafeBind (ResolverS res) m2 = ResolverS $ do
  (readResA :: ReaderT e (Resolver QUERY e m) a) <- res
  pure $ ReaderT $ \e -> ResolverQ $ do
    let (resA :: Resolver QUERY e m a) = runReaderT readResA e
    (valA :: a) <- runResolverQ resA
    (readResB :: ReaderT e (Resolver QUERY e m) b) <- clearStateResolverEvents $ runResolverS (m2 valA)
    runResolverQ $ runReaderT readResB e

subscribe ::
  forall e m a.
  ( PushEvents (Channel e) (ResolverState (Channel e) m),
    Monad m
  ) =>
  [StreamChannel e] ->
  Resolver QUERY e m (e -> Resolver QUERY e m a) ->
  Resolver SUBSCRIPTION e m a
subscribe ch res = ResolverS $ do
  pushEvents (map Channel ch :: [Channel e])
  (eventRes :: e -> Resolver QUERY e m a) <- clearStateResolverEvents (runResolverQ res)
  pure $ ReaderT eventRes

unsafeInternalContext :: (Monad m, LiftOperation o) => Resolver o e m Context
unsafeInternalContext = packResolver $ ResolverState ask

-- Converts Subscription Resolver Type to Query Resolver
type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (Resolver SUBSCRIPTION e m) = Resolver QUERY e m

-- map Resolving strategies
class
  MapStrategy
    (from :: OperationType)
    (to :: OperationType)
  where
  mapStrategy ::
    Monad m =>
    Resolver from e m (Deriving from e m) ->
    Resolver to e m (Deriving to e m)

instance MapStrategy o o where
  mapStrategy = id

data Deriving (o :: OperationType) e (m :: * -> *)
  = DerivingNull
  | DerivingScalar ScalarValue
  | DerivingEnum Name Name
  | DerivingList [Deriving o e m]
  | DerivingObject (ObjectDeriving o e m)
  | DerivingUnion Name (Resolver o e m (Deriving o e m))
  deriving (Show)

data ObjectDeriving o e m = ObjectDeriving
  { __typename :: Name,
    objectFields ::
      [(Name, FieldDeriving o e m)]
  }
  deriving (Show)

newtype FieldDeriving o e m = FieldDeriving {runFieldDeriving :: Arguments VALID -> Resolver o e m (Deriving o e m)}

-- | FieldDerivingWithArgs

-- instance KeyOf (FieldDeriving o e m) where
--   keyOf = fieldName

instance Show (FieldDeriving o e m) where
  show _ = "FieldDeriving"

instance MapStrategy QUERY SUBSCRIPTION where
  mapStrategy = ResolverS . pure . lift . fmap mapDeriving

mapDeriving ::
  ( MapStrategy o o',
    Monad m
  ) =>
  Deriving o e m ->
  Deriving o' e m
mapDeriving DerivingNull = DerivingNull
mapDeriving (DerivingScalar x) = DerivingScalar x
mapDeriving (DerivingEnum typeName enum) = DerivingEnum typeName enum
mapDeriving (DerivingList x) = DerivingList $ map mapDeriving x
mapDeriving (DerivingObject x) = DerivingObject (mapObjectDeriving x)
mapDeriving (DerivingUnion name x) = DerivingUnion name (mapStrategy x)

mapObjectDeriving ::
  ( MapStrategy o o',
    Monad m
  ) =>
  ObjectDeriving o e m ->
  ObjectDeriving o' e m
mapObjectDeriving (ObjectDeriving tyname fields) =
  ObjectDeriving tyname $
    map (mapFieldDeriving mapStrategy) fields

mapFieldDeriving ::
  ( Resolver from e m (Deriving from e m) ->
    Resolver to e m (Deriving to e m)
  ) ->
  (Name, FieldDeriving from e m) ->
  (Name, FieldDeriving to e m)
--mapFieldDeriving f (name, FieldDeriving value) = (name, FieldDeriving $ f value)
mapFieldDeriving f (name, FieldDeriving value) = (name, FieldDeriving $ \x -> f (value x))

--
-- Selection Processing
toResolver ::
  forall o e m a b.
  (LiftOperation o, Monad m) =>
  Eventless a ->
  (a -> Resolver o e m b) ->
  Resolver o e m b
toResolver args = withResolver resState
  where
    resState :: ResolverState e m a
    resState = ResolverState $ lift $ cleanEvents $ ResultT $ pure args

-- DataResolver
instance Merge (Deriving o e m) where
  merge p (DerivingObject x) (DerivingObject y) =
    DerivingObject <$> merge p x y
  merge _ _ _ =
    failure $ internalResolvingError "can't merge: incompatible resolvers"

instance Merge (ObjectDeriving o e m) where
  merge _ (ObjectDeriving tyname x) (ObjectDeriving _ y) =
    pure $ ObjectDeriving tyname (x <> y)

pickSelection :: Name -> UnionSelection -> SelectionSet VALID
pickSelection = selectOr empty unionTagSelection

withObject ::
  (LiftOperation o, Monad m) =>
  (SelectionSet VALID -> Resolver o e m value) ->
  Selection VALID ->
  Resolver o e m value
withObject f Selection {selectionName, selectionContent, selectionPosition} = checkContent selectionContent
  where
    checkContent (SelectionSet selection) = f selection
    checkContent _ = failure (subfieldsNotSelected selectionName "" selectionPosition)

lookupRes ::
  (LiftOperation o, Monad m) =>
  Selection VALID ->
  ObjectDeriving o e m ->
  Resolver o e m ValidValue
lookupRes Selection {selectionName, selectionArguments}
  | selectionName == "__typename" =
    pure . Scalar . String . __typename
  | otherwise =
    maybe
      (pure gqlNull)
      toRes
      . lookup selectionName
      . objectFields
  where
    toRes (FieldDeriving f) = f selectionArguments `unsafeBind` runDataResolver

resolveObject ::
  forall o e m.
  (LiftOperation o, Monad m) =>
  SelectionSet VALID ->
  Deriving o e m ->
  Resolver o e m ValidValue
resolveObject selectionSet (DerivingObject drv@ObjectDeriving {__typename}) =
  Object . toOrderedMap <$> traverse resolver selectionSet
  where
    resolver :: Selection VALID -> Resolver o e m (ObjectEntry VALID)
    resolver sel =
      setSelection sel
        $ setTypeName __typename
        $ ObjectEntry (keyOf sel) <$> lookupRes sel drv
resolveObject _ _ =
  failure $ internalResolvingError "expected object as resolver"

toEventResolver :: Monad m => ReaderT event (Resolver QUERY event m) ValidValue -> Context -> event -> m GQLResponse
toEventResolver (ReaderT subRes) sel event = do
  value <- runResultT $ runReaderT (runResolverState $ runResolverQ (subRes event)) sel
  pure $ renderResponse value

runDataResolver :: (Monad m, LiftOperation o) => Deriving o e m -> Resolver o e m ValidValue
runDataResolver = withResolver getState . __encode
  where
    __encode obj sel@Selection {selectionContent} = encodeNode obj selectionContent
      where
        -- LIST
        encodeNode (DerivingList x) _ = List <$> traverse runDataResolver x
        -- Object -----------------
        encodeNode objDrv@DerivingObject {} _ = withObject (`resolveObject` objDrv) sel
        -- ENUM
        encodeNode (DerivingEnum _ enum) SelectionField = pure $ gqlString enum
        encodeNode (DerivingEnum typename enum) unionSel@UnionSelection {} =
          encodeNode (unionDrv (typename <> "EnumObject")) unionSel
          where
            unionDrv name =
              DerivingUnion name
                $ pure
                $ DerivingObject
                $ ObjectDeriving name [("enum", FieldDeriving $ const $ pure $ DerivingScalar $ String enum)]
        encodeNode DerivingEnum {} _ =
          failure ("wrong selection on enum value" :: Message)
        -- UNION
        encodeNode (DerivingUnion typename unionRef) (UnionSelection selections) =
          unionRef >>= resolveObject currentSelection
          where
            currentSelection = pickSelection typename selections
        encodeNode (DerivingUnion name _) _ =
          failure ("union Resolver \"" <> name <> "\" should only recieve UnionSelection" :: Message)
        -- SCALARS
        encodeNode DerivingNull _ = pure Null
        encodeNode (DerivingScalar x) SelectionField = pure $ Scalar x
        encodeNode DerivingScalar {} _ =
          failure ("scalar Resolver should only recieve SelectionField" :: Message)

runResolver ::
  Monad m =>
  Resolver o event m ValidValue ->
  Context ->
  ResponseStream event m ValidValue
runResolver (ResolverQ resT) sel = cleanEvents $ (runReaderT $ runResolverState resT) sel
runResolver (ResolverM resT) sel = mapEvent Publish $ (runReaderT $ runResolverState resT) sel
runResolver (ResolverS resT) sel = ResultT $ do
  (readResValue :: Result (Channel event1) (ReaderT event (Resolver QUERY event m) ValidValue)) <- runResultT $ (runReaderT $ runResolverState resT) sel
  pure $ case readResValue of
    Failure x -> Failure x
    Success {warnings, result, events = channels} -> do
      let eventRes = toEventResolver result sel
      Success
        { events = [Subscribe $ Event channels eventRes],
          warnings,
          result = gqlNull
        }

runRootDataResolver ::
  (Monad m, LiftOperation o) =>
  Eventless (Deriving o e m) ->
  Context ->
  ResponseStream e m (Value VALID)
runRootDataResolver
  res
  ctx@Context {operation = Operation {operationSelection}} =
    do
      root <- statelessToResultT res
      runResolver (resolveObject operationSelection root) ctx

-------------------------------------------------------------------

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you can use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> *) (mut :: (* -> *) -> *) (sub :: (* -> *) -> *) = GQLRootResolver
  { queryResolver :: query (Resolver QUERY event m),
    mutationResolver :: mut (Resolver MUTATION event m),
    subscriptionResolver :: sub (Resolver SUBSCRIPTION event m)
  }

data ResolverModel e m = ResolverModel
  { query :: Eventless (Deriving QUERY e m),
    mutation :: Eventless (Deriving MUTATION e m),
    subscription :: Eventless (Deriving SUBSCRIPTION e m)
  }

runResolverModel :: Monad m => ResolverModel e m -> Context -> ResponseStream e m (Value VALID)
runResolverModel
  ResolverModel
    { query,
      mutation,
      subscription
    }
  ctx@Context {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation Query =
        runRootDataResolver query ctx
      selectByOperation Mutation =
        runRootDataResolver mutation ctx
      selectByOperation Subscription =
        runRootDataResolver subscription ctx
