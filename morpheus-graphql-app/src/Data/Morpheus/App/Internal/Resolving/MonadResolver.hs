{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.MonadResolver
  ( MonadResolver (..),
    MonadIOResolver,
    SubscriptionField (..),
    ResolverContext (..),
    withArguments,
    getArgument,
  )
where

import Control.Monad.Except (MonadError)
import Data.Morpheus.App.Internal.Resolving.Event
  ( EventHandler (..),
  )
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverState,
  )
import Data.Morpheus.Internal.Utils (selectOr)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    FieldName,
    GQLError,
    MUTATION,
    OperationType,
    SUBSCRIPTION,
    VALID,
    Value (..),
  )
import Relude

class (MonadResolver m, MonadIO m) => MonadIOResolver (m :: Type -> Type)

class (Monad m, MonadReader ResolverContext m, MonadFail m, MonadError GQLError m) => MonadResolver (m :: Type -> Type) where
  type MonadOperation m :: OperationType
  type MonadEvent m :: Type
  type MonadQuery m :: (Type -> Type)
  type MonadMutation m :: (Type -> Type)
  type MonadSubscription m :: (Type -> Type)
  liftState :: ResolverState a -> m a
  getArguments :: m (Arguments VALID)
  subscribe :: (MonadOperation m ~ SUBSCRIPTION) => Channel (MonadEvent m) -> MonadQuery m (MonadEvent m -> m a) -> SubscriptionField (m a)
  publish :: (MonadOperation m ~ MUTATION) => [MonadEvent m] -> m ()

data SubscriptionField (a :: Type) where
  SubscriptionField ::
    { channel :: forall m v. (a ~ m v, MonadResolver m, MonadOperation m ~ SUBSCRIPTION) => Channel (MonadEvent m),
      unSubscribe :: a
    } ->
    SubscriptionField a

withArguments :: (MonadResolver m) => (Arguments VALID -> m a) -> m a
withArguments = (getArguments >>=)

getArgument :: (MonadResolver m) => FieldName -> m (Value VALID)
getArgument name = selectOr Null argumentValue name <$> getArguments
