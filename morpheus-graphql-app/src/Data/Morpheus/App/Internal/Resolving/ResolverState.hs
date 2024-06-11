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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverStateT (..),
    resolverFailureMessage,
    clearStateResolverEvents,
    ResolverState,
    toResolverStateT,
    runResolverStateT,
    runResolverStateM,
    runResolverState,
    runResolverStateValueM,
    updateCurrentType,
    askFieldTypeName,
    inField,
    inSelectionField,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Reader (mapReaderT)
import Data.Morpheus.Core
  ( Config (..),
    RenderGQL,
    render,
  )
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    PushEvents (..),
    Result,
    ResultT (..),
    cleanEvents,
  )
import Data.Morpheus.Internal.Utils (selectOr)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition (fieldType),
    FieldName,
    GQLError,
    Operation,
    Schema,
    Selection (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (typeConName),
    VALID,
    at,
    internal,
    isInternal,
    lookupDataType,
    msg,
  )
import Relude

data ResolverContext = ResolverContext
  { currentSelection :: Selection VALID,
    schema :: Schema VALID,
    operation :: Operation VALID,
    config :: Config,
    currentType :: TypeDefinition ANY VALID
  }
  deriving (Show)

updateCurrentType ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  Maybe TypeName ->
  m a ->
  m a
updateCurrentType = maybe id setCurrentType

setCurrentType ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  TypeName ->
  m a ->
  m a
setCurrentType name ma = do
  t <- asks (lookupDataType name . schema)
  maybe
    (const $ throwError $ internal "Unknown type \"" <> msg name <> "\".")
    (\currentType -> local (\ctx -> ctx {currentType}))
    t
    ma

fieldTypeName :: FieldName -> TypeDefinition ANY VALID -> Maybe TypeName
fieldTypeName name t = case typeContent t of
  (DataObject _ fs) -> selectOr Nothing (Just . typeConName . fieldType) name fs
  (DataInterface fs) -> selectOr Nothing (Just . typeConName . fieldType) name fs
  _ -> Nothing

inSelectionField :: (MonadReader ResolverContext m, MonadError GQLError m) => Selection VALID -> m b -> m b
inSelectionField selection m = do
  inField (selectionName selection)
    $ local (\ctx -> ctx {currentSelection = selection}) m

inField :: (MonadReader ResolverContext m, MonadError GQLError m) => FieldName -> m b -> m b
inField fieldName m = do
  fieldType <- askFieldTypeName fieldName
  updateCurrentType fieldType m

askFieldTypeName :: (MonadReader ResolverContext m) => FieldName -> m (Maybe TypeName)
askFieldTypeName name = asks (fieldTypeName name . currentType)

type ResolverState = ResolverStateT () Identity

runResolverStateT :: ResolverStateT e m a -> ResolverContext -> ResultT e m a
runResolverStateT = runReaderT . _runResolverStateT

runResolverStateM :: ResolverStateT e m a -> ResolverContext -> m (Result GQLError ([e], a))
runResolverStateM res = runResultT . runResolverStateT res

runResolverStateValueM :: (Functor m) => ResolverStateT e m a -> ResolverContext -> m (Result GQLError a)
runResolverStateValueM res = fmap (fmap snd) . runResolverStateM res

runResolverState :: ResolverState a -> ResolverContext -> GQLResult a
runResolverState res = fmap snd . runIdentity . runResolverStateM res

-- internal resolver state
newtype ResolverStateT event m a = ResolverStateT
  { _runResolverStateT :: ReaderT ResolverContext (ResultT event m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ResolverContext
    )

instance MonadTrans (ResolverStateT e) where
  lift = ResolverStateT . lift . lift

instance (Monad m) => MonadError GQLError (ResolverStateT e m) where
  throwError err = do
    ctx <- asks id
    let f = if isInternal err then renderInternalResolverError ctx else resolverFailureMessage ctx
    ResolverStateT
      $ lift
      $ throwError
      $ f err
  catchError (ResolverStateT mx) f = ResolverStateT $ catchError mx (_runResolverStateT . f)

instance (Monad m) => PushEvents e (ResolverStateT e m) where
  pushEvents = ResolverStateT . lift . pushEvents

mapResolverState ::
  ( ResultT e m a ->
    ResultT e' m' a'
  ) ->
  ResolverStateT e m a ->
  ResolverStateT e' m' a'
mapResolverState f (ResolverStateT x) = ResolverStateT (mapReaderT f x)

toResolverStateT ::
  (Applicative m) =>
  ResolverState a ->
  ResolverStateT e m a
toResolverStateT = mapResolverState injectResult

injectResult ::
  (Applicative m) =>
  ResultT () Identity a ->
  ResultT e m a
injectResult (ResultT (Identity x)) =
  cleanEvents $ ResultT (pure x)

-- clear events and starts new resolver with different type of events but with same value
-- use properly. only if you know what you are doing
clearStateResolverEvents :: (Functor m) => ResolverStateT e m a -> ResolverStateT e' m a
clearStateResolverEvents = mapResolverState cleanEvents

resolverFailureMessage :: ResolverContext -> GQLError -> GQLError
resolverFailureMessage
  ctx@ResolverContext
    { currentSelection =
        Selection {selectionName, selectionPosition}
    }
  err =
    "Failure on Resolving Field "
      <> msg selectionName
      <> ": "
      <> err
      <> withInternalContext ctx
      `at` selectionPosition

renderInternalResolverError :: ResolverContext -> GQLError -> GQLError
renderInternalResolverError ctx@ResolverContext {currentSelection} err =
  internal
    $ (err <> ". " <> msg (renderContext ctx))
    `at` selectionPosition currentSelection

withInternalContext :: ResolverContext -> GQLError
withInternalContext ResolverContext {config = Config {debug = False}} = ""
withInternalContext resCTX = renderContext resCTX

renderContext :: ResolverContext -> GQLError
renderContext
  ResolverContext
    { currentSelection,
      schema,
      operation,
      currentType
    } =
    renderSection "Current Type" (typeName currentType)
      <> renderSection "Current Selection" currentSelection
      <> renderSection "OperationDefinition" operation
      <> renderSection "SchemaDefinition" schema

renderSection :: (RenderGQL a) => GQLError -> a -> GQLError
renderSection label content =
  "\n\n"
    <> label
    <> ":\n"
    <> line
    <> "\n\n"
    <> msg (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
