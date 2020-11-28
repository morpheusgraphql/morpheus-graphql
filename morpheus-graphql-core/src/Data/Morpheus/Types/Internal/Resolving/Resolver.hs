{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving.Resolver
  ( Resolver,
    LiftOperation,
    lift,
    subscribe,
    ResponseEvent (..),
    ResponseStream,
    ObjectResModel (..),
    ResModel (..),
    WithOperation,
    ResolverContext (..),
    unsafeInternalContext,
    runRootResModel,
    RootResModel (..),
    withArguments,
    getArguments,
    SubscriptionField (..),
    liftResolverState,
    mkObject,
    mkUnion,
    FieldResModel,
    mkEnum,
  )
where

import Control.Monad.Trans.Reader (mapReaderT)
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Error.Selection (subfieldsNotSelected)
import Data.Morpheus.Ext.SemigroupM (SemigroupM (..))
import Data.Morpheus.Internal.Utils
  ( empty,
    keyOf,
    selectOr,
    traverseCollection,
  )
import Data.Morpheus.Types.IO
  ( GQLResponse,
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( Arguments,
    FieldName,
    GQLValue (..),
    InternalError,
    MUTATION,
    Message,
    ObjectEntry (..),
    Operation (..),
    OperationType,
    OperationType (..),
    QUERY,
    Ref,
    SUBSCRIPTION,
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeName (..),
    UnionSelection,
    UnionTag (..),
    VALID,
    ValidValue,
    Value (..),
    msg,
    toGQLError,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Eventless,
    Failure (..),
    PushEvents (..),
    Result (..),
    ResultT (..),
    cleanEvents,
    mapEvent,
  )
import Data.Morpheus.Types.Internal.Resolving.Event
  ( EventHandler (..),
    ResponseEvent (..),
  )
import Data.Morpheus.Types.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverState,
    ResolverStateT (..),
    clearStateResolverEvents,
    resolverFailureMessage,
    runResolverState,
    runResolverStateM,
    runResolverStateT,
    toResolverStateT,
  )
import Relude hiding
  ( Show,
    empty,
    show,
  )
import Prelude (Show (..))

type WithOperation (o :: OperationType) = LiftOperation o

type ResponseStream event (m :: * -> *) = ResultT (ResponseEvent event m) m

data SubscriptionField (a :: *) where
  SubscriptionField ::
    { channel :: forall e m v. a ~ Resolver SUBSCRIPTION e m v => Channel e,
      unSubscribe :: a
    } ->
    SubscriptionField a

--
-- GraphQL Field Resolver
--
---------------------------------------------------------------
data Resolver (o :: OperationType) event (m :: * -> *) value where
  ResolverQ :: {runResolverQ :: ResolverStateT () m value} -> Resolver QUERY event m value
  ResolverM :: {runResolverM :: ResolverStateT event m value} -> Resolver MUTATION event m value
  ResolverS :: {runResolverS :: ResolverStateT () m (SubEventRes event m value)} -> Resolver SUBSCRIPTION event m value

type SubEventRes event m value = ReaderT event (ResolverStateT () m) value

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
  (ResolverQ x) >>= m2 = ResolverQ (x >>= runResolverQ . m2)
  (ResolverM x) >>= m2 = ResolverM (x >>= runResolverM . m2)
  (ResolverS res) >>= m2 = ResolverS (liftSubResolver m2 <$> res)

liftSubResolver ::
  (Monad m) =>
  (t -> Resolver SUBSCRIPTION r m a) ->
  ReaderT r (ResolverStateT () m) t ->
  ReaderT r (ResolverStateT () m) a
liftSubResolver m2 readResA = ReaderT $ \e -> do
  a <- runReaderT readResA e
  readResB <- runResolverS (m2 a)
  runReaderT readResB e

-- MonadIO
instance (MonadIO m, LiftOperation o) => MonadIO (Resolver o e m) where
  liftIO = lift . liftIO

-- Monad Transformers
instance (LiftOperation o) => MonadTrans (Resolver o e) where
  lift = packResolver . lift

-- Failure
instance (LiftOperation o, Monad m, Failure err (ResolverStateT e m)) => Failure err (Resolver o e m) where
  failure = packResolver . failure

instance (Monad m, LiftOperation o) => MonadFail (Resolver o e m) where
  fail = failure . msg

-- PushEvents
instance (Monad m) => PushEvents e (Resolver MUTATION e m) where
  pushEvents = packResolver . pushEvents

instance (Monad m, Semigroup a, LiftOperation o) => Semigroup (Resolver o e m a) where
  x <> y = fmap (<>) x <*> y

instance (LiftOperation o, Monad m) => MonadReader ResolverContext (Resolver o e m) where
  ask = packResolver ask
  local f (ResolverQ res) = ResolverQ (local f res)
  local f (ResolverM res) = ResolverM (local f res)
  local f (ResolverS resM) = ResolverS $ mapReaderT (local f) <$> resM

-- | A function to return the internal 'ResolverContext' within a resolver's monad.
-- Using the 'ResolverContext' itself is unsafe because it expposes internal structures
-- of the AST, but you can use the "Data.Morpheus.Types.SelectionTree" typeclass to manipulate
-- the internal AST with a safe interface.
unsafeInternalContext :: (Monad m, LiftOperation o) => Resolver o e m ResolverContext
unsafeInternalContext = ask

liftResolverState :: (LiftOperation o, Monad m) => ResolverState a -> Resolver o e m a
liftResolverState = packResolver . toResolverStateT

class LiftOperation (o :: OperationType) where
  packResolver :: Monad m => ResolverStateT e m a -> Resolver o e m a

instance LiftOperation QUERY where
  packResolver = ResolverQ . clearStateResolverEvents

instance LiftOperation MUTATION where
  packResolver = ResolverM

instance LiftOperation SUBSCRIPTION where
  packResolver = ResolverS . pure . lift . clearStateResolverEvents

subscribe ::
  (Monad m) =>
  Channel e ->
  Resolver QUERY e m (e -> Resolver SUBSCRIPTION e m a) ->
  SubscriptionField (Resolver SUBSCRIPTION e m a)
subscribe ch res =
  SubscriptionField ch
    $ ResolverS
    $ fromSub <$> runResolverQ res
  where
    fromSub :: Monad m => (e -> Resolver SUBSCRIPTION e m a) -> ReaderT e (ResolverStateT () m) a
    fromSub f = join (ReaderT (runResolverS . f))

withArguments ::
  (LiftOperation o, Monad m) =>
  (Arguments VALID -> Resolver o e m a) ->
  Resolver o e m a
withArguments = (getArguments >>=)

getArguments ::
  (LiftOperation o, Monad m) =>
  Resolver o e m (Arguments VALID)
getArguments = selectionArguments . currentSelection <$> unsafeInternalContext

pickSelection :: TypeName -> UnionSelection VALID -> SelectionSet VALID
pickSelection = selectOr empty unionTagSelection

withObject ::
  (LiftOperation o, Monad m) =>
  TypeName ->
  (SelectionSet VALID -> Resolver o e m value) ->
  Selection VALID ->
  Resolver o e m value
withObject __typename f Selection {selectionName, selectionContent, selectionPosition} = checkContent selectionContent
  where
    checkContent (SelectionSet selection) = f selection
    checkContent (UnionSelection unionSel) =
      f (selectOr empty unionTagSelection __typename unionSel)
    checkContent _ = failure [toGQLError $ subfieldsNotSelected selectionName "" selectionPosition]

lookupRes ::
  (LiftOperation o, Monad m) =>
  Selection VALID ->
  ObjectResModel o e m ->
  Resolver o e m ValidValue
lookupRes Selection {selectionName}
  | selectionName == "__typename" =
    pure . Scalar . String . readTypeName . __typename
  | otherwise =
    maybe
      (pure gqlNull)
      (>>= runDataResolver)
      . HM.lookup selectionName
      . objectFields

type FieldResModel o e m = (FieldName, Resolver o e m (ResModel o e m))

mkObject ::
  TypeName ->
  [FieldResModel o e m] ->
  ResModel o e m
mkObject __typename fields =
  ResObject
    ( ObjectResModel
        { __typename,
          objectFields = HM.fromList fields
        }
    )

mkUnion ::
  (LiftOperation o, Monad m) =>
  TypeName ->
  [FieldResModel o e m] ->
  ResModel o e m
mkUnion name =
  ResUnion
    name
    . pure
    . mkObject
      name

mkEnum :: TypeName -> ResModel o e m
mkEnum = ResEnum

mkEnumNull :: (LiftOperation o, Monad m) => [FieldResModel o e m]
mkEnumNull = [("empty", pure $ mkEnum "Empty")]

resolveObject ::
  forall o e m.
  (LiftOperation o, Monad m) =>
  SelectionSet VALID ->
  ResModel o e m ->
  Resolver o e m ValidValue
resolveObject selectionSet (ResObject drv@ObjectResModel {__typename}) =
  Object <$> traverseCollection resolver selectionSet
  where
    resolver :: Selection VALID -> Resolver o e m (ObjectEntry VALID)
    resolver currentSelection =
      local (\ctx -> ctx {currentSelection, currentTypeName = __typename}) $
        ObjectEntry (keyOf currentSelection) <$> lookupRes currentSelection drv
resolveObject _ _ = packResolver $ failure ("expected object as resolver" :: InternalError)

runDataResolver :: (Monad m, LiftOperation o) => ResModel o e m -> Resolver o e m ValidValue
runDataResolver res = asks currentSelection >>= __encode res
  where
    __encode obj sel@Selection {selectionContent} = encodeNode obj selectionContent
      where
        -- LIST
        encodeNode (ResList x) _ = List <$> traverse runDataResolver x
        -- Object -----------------
        encodeNode objDrv@(ResObject ObjectResModel {__typename}) _ = withObject __typename (`resolveObject` objDrv) sel
        -- ENUM
        encodeNode (ResEnum enum) SelectionField = pure $ gqlString $ readTypeName enum
        encodeNode (ResEnum name) unionSel@UnionSelection {} =
          encodeNode (mkUnion name mkEnumNull) unionSel
        encodeNode ResEnum {} _ = failure ("wrong selection on enum value" :: Message)
        -- UNION
        encodeNode (ResUnion typename unionRef) (UnionSelection selections) =
          unionRef >>= resolveObject currentSelection
          where
            currentSelection = pickSelection typename selections
        encodeNode (ResUnion name _) _ =
          failure ("union Resolver " <> msg name <> " should only recieve UnionSelection")
        -- SCALARS
        encodeNode ResNull _ = pure Null
        encodeNode (ResScalar x) SelectionField = pure $ Scalar x
        encodeNode ResScalar {} _ =
          failure ("scalar Resolver should only recieve SelectionField" :: Message)

runResolver ::
  Monad m =>
  Maybe (Selection VALID -> ResolverState (Channel event)) ->
  Resolver o event m ValidValue ->
  ResolverContext ->
  ResponseStream event m ValidValue
runResolver _ (ResolverQ resT) sel = cleanEvents $ runResolverStateT resT sel
runResolver _ (ResolverM resT) sel = mapEvent Publish $ runResolverStateT resT sel
runResolver toChannel (ResolverS resT) ctx = ResultT $ do
  readResValue <- runResolverStateM resT ctx
  pure $ case readResValue >>= subscriptionEvents ctx toChannel . toEventResolver ctx of
    Failure x -> Failure x
    Success {warnings, result} ->
      Success
        { events = [result],
          warnings,
          result = gqlNull
        }

toEventResolver :: Monad m => ResolverContext -> SubEventRes event m ValidValue -> (event -> m GQLResponse)
toEventResolver sel (ReaderT subRes) event = renderResponse <$> runResolverStateM (subRes event) sel

subscriptionEvents ::
  ResolverContext ->
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  (e -> m GQLResponse) ->
  Eventless (ResponseEvent e m)
subscriptionEvents ctx@ResolverContext {currentSelection} (Just channelGenerator) res =
  runResolverState handle ctx
  where
    handle = do
      channel <- channelGenerator currentSelection
      pure $ Subscribe channel res
subscriptionEvents ctx Nothing _ = failure [resolverFailureMessage ctx "channel Resolver is not defined"]

-- Resolver Models -------------------------------------------------------------------

data ObjectResModel o e m = ObjectResModel
  { __typename :: TypeName,
    objectFields :: HashMap FieldName (Resolver o e m (ResModel o e m))
  }
  deriving (Show)

instance
  ( Monad m,
    Applicative f,
    LiftOperation o
  ) =>
  SemigroupM f (ObjectResModel o e m)
  where
  mergeM path (ObjectResModel tyname x) (ObjectResModel _ y) =
    pure $ ObjectResModel tyname (HM.unionWith (mergeResolver path) x y)

mergeResolver ::
  (Monad m, SemigroupM (ResolverStateT e m) a, LiftOperation o) =>
  [Ref] ->
  Resolver o e m a ->
  Resolver o e m a ->
  Resolver o e m a
mergeResolver path a b = do
  a' <- a
  b >>= packResolver . mergeM path a'

data ResModel (o :: OperationType) e (m :: * -> *)
  = ResNull
  | ResScalar ScalarValue
  | ResEnum TypeName
  | ResObject (ObjectResModel o e m)
  | ResList [ResModel o e m]
  | ResUnion TypeName (Resolver o e m (ResModel o e m))
  deriving (Show)

instance
  ( Monad f,
    Monad m,
    LiftOperation o,
    Failure InternalError f
  ) =>
  SemigroupM f (ResModel o e m)
  where
  mergeM _ ResNull ResNull = pure ResNull
  mergeM _ ResScalar {} x@ResScalar {} = pure x
  mergeM _ ResEnum {} x@ResEnum {} = pure x
  mergeM p (ResObject x) (ResObject y) = ResObject <$> mergeM p x y
  mergeM _ _ _ = failure ("can't merge: incompatible resolvers" :: InternalError)

data RootResModel e m = RootResModel
  { query :: ResolverState (ResModel QUERY e m),
    mutation :: ResolverState (ResModel MUTATION e m),
    subscription :: ResolverState (ResModel SUBSCRIPTION e m),
    channelMap :: Maybe (Selection VALID -> ResolverState (Channel e))
  }

runRootDataResolver ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  ResolverState (ResModel o e m) ->
  ResolverContext ->
  ResponseStream e m (Value VALID)
runRootDataResolver
  channels
  res
  ctx@ResolverContext {operation = Operation {operationSelection}} =
    do
      root <- runResolverStateT (toResolverStateT res) ctx
      runResolver channels (resolveObject operationSelection root) ctx

runRootResModel :: Monad m => RootResModel e m -> ResolverContext -> ResponseStream e m (Value VALID)
runRootResModel
  RootResModel
    { query,
      mutation,
      subscription,
      channelMap
    }
  ctx@ResolverContext {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation Query =
        runRootDataResolver channelMap query ctx
      selectByOperation Mutation =
        runRootDataResolver channelMap mutation ctx
      selectByOperation Subscription =
        runRootDataResolver channelMap subscription ctx
