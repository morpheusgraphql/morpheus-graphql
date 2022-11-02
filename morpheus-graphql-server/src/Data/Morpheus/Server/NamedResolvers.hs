{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.NamedResolvers
  ( ResolveNamed (..),
    NamedResolverT (..),
    resolve,
    useBatched,
    Dependency,
    ignoreBatching,
  )
where

import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Morpheus.Types.ID (ID)
import Data.Morpheus.Types.Internal.AST (GQLError, internal)
import Relude

type family Target a :: Type where
  Target [a] = a
  Target (Maybe a) = a
  Target a = a

type family Dependency a :: Type where
  Dependency [a] = Dependency a
  Dependency (Maybe a) = Dependency a
  Dependency ID = ID
  Dependency Text = Text
  Dependency a = Dep a

ignoreBatching :: (Monad m) => (a -> m b) -> [a] -> m [Maybe b]
ignoreBatching f = traverse (fmap Just . f)

{-# DEPRECATED useBatched " this function is obsolete" #-}
useBatched :: (ResolveNamed m a, MonadError GQLError m) => Dependency a -> m a
useBatched x = resolveBatched [x] >>= res
  where
    res [Just v] = pure v
    res _ = throwError (internal "named resolver should return single value for single argument")

{-# DEPRECATED resolveNamed "use: resolveBatched" #-}

class ToJSON (Dependency a) => ResolveNamed (m :: Type -> Type) (a :: Type) where
  type Dep a :: Type
  resolveBatched :: MonadError GQLError m => [Dependency a] -> m [Maybe a]

  resolveNamed :: MonadError GQLError m => Dependency a -> m a
  resolveNamed = useBatched

data NamedResolverT (m :: Type -> Type) a where
  Ref :: ResolveNamed m (Target a) => m (Dependency a) -> NamedResolverT m a
  Refs :: ResolveNamed m (Target a) => m [Dependency a] -> NamedResolverT m [a]
  Value :: m a -> NamedResolverT m a

data RES = LIST | REF

type family RES_TYPE b :: RES where
  RES_TYPE [b] = 'LIST
  RES_TYPE b = 'REF

instance MonadTrans NamedResolverT where
  lift = Value

resolve :: forall m a b. (ResolveByType (RES_TYPE b) m a b) => Monad m => m a -> NamedResolverT m b
resolve = resolveByType (Proxy :: Proxy (RES_TYPE b))

class ResolveByType (k :: RES) m a b where
  resolveByType :: Monad m => f k -> m a -> NamedResolverT m b

instance (ResolveNamed m (Target b), a ~ Dependency b) => ResolveByType 'LIST m [a] [b] where
  resolveByType _ = Refs

instance (ResolveNamed m (Target b), Dependency b ~ a) => ResolveByType 'REF m a b where
  resolveByType _ = Ref
