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
import Data.Vector (Vector)
import GHC.TypeLits (Symbol)
import Relude

type family Target a :: Type where
  Target (Maybe a) = a
  Target [a] = a
  Target (Set a) = a
  Target (NonEmpty a) = a
  Target (Seq a) = a
  Target (Vector a) = a
  Target a = a

type family Dependency a :: Type where
-- scalars
  Dependency Int = Int
  Dependency Double = Double
  Dependency Float = Float
  Dependency Text = Text
  Dependency Bool = Bool
  Dependency ID = ID
-- wrappers
  Dependency (Maybe a) = Dependency a
  Dependency [a] = Dependency a
  Dependency (Set a) = Dependency a
  Dependency (NonEmpty a) = Dependency a
  Dependency (Seq a) = Dependency a
  Dependency (Vector a) = Dependency a
-- custom
  Dependency a = Dep a

ignoreBatching :: (Monad m) => (a -> m b) -> [a] -> m [Maybe b]
ignoreBatching f = traverse (fmap Just . f)

forward :: (Monad m, Dependency a ~ a) => [Dependency a] -> m [Maybe a]
forward = pure . map Just

{-# DEPRECATED useBatched " this function is obsolete" #-}
useBatched :: (ResolveNamed m a, MonadError GQLError m) => Dependency a -> m a
useBatched x = resolveBatched [x] >>= res
  where
    res [Just v] = pure v
    res _ = throwError (internal "named resolver should return single value for single argument")

{-# DEPRECATED resolveNamed "use: resolveBatched" #-}

instance ResolveNamed m Text where
  type Dep Text = Text
  resolveBatched = forward

instance ResolveNamed m ID where
  type Dep ID = ID
  resolveBatched = forward

class ToJSON (Dependency a) => ResolveNamed (m :: Type -> Type) (a :: Type) where
  type Dep a :: Type
  resolveBatched :: MonadError GQLError m => [Dependency a] -> m [Maybe a]

  resolveNamed :: MonadError GQLError m => Dependency a -> m a
  resolveNamed = useBatched

data NamedResolverT (m :: Type -> Type) a where
  Ref :: ResolveNamed m (Target a) => m (Dependency a) -> NamedResolverT m a
  Refs :: ResolveNamed m (Target a) => m [Dependency a] -> NamedResolverT m [a]
  Value :: m a -> NamedResolverT m a

data TargetType = LIST | SINGLE | ERROR Symbol

type family NamedResolverTarget b :: TargetType where
  NamedResolverTarget [a] = 'LIST
  NamedResolverTarget (Set a) = 'LIST
  NamedResolverTarget (NonEmpty a) = 'LIST
  NamedResolverTarget (Seq a) = 'LIST
  NamedResolverTarget (Vector a) = 'LIST
  NamedResolverTarget Int = 'ERROR "use lift, type Int can't have ResolveNamed instance"
  NamedResolverTarget Double = 'ERROR "use lift, type Double can't have ResolveNamed instance"
  NamedResolverTarget Float = 'ERROR "use lift, type Float can't have ResolveNamed instance"
  NamedResolverTarget Text = 'ERROR "use lift, type Text can't have ResolveNamed instance"
  NamedResolverTarget Bool = 'ERROR "use lift, type Bool can't have ResolveNamed instance"
  NamedResolverTarget ID = 'ERROR "use lift, type ID can't have ResolveNamed instance"
  NamedResolverTarget b = 'SINGLE

instance MonadTrans NamedResolverT where
  lift = Value

resolve :: forall m a b. (ResolveRef (NamedResolverTarget b) m a b) => Monad m => m a -> NamedResolverT m b
resolve = resolveRef (Proxy :: Proxy (NamedResolverTarget b))

class ResolveRef (k :: TargetType) m a b where
  resolveRef :: Monad m => f k -> m a -> NamedResolverT m b

instance (ResolveNamed m (Target b), a ~ Dependency b) => ResolveRef 'LIST m [a] [b] where
  resolveRef _ = Refs

instance (ResolveNamed m (Target b), Dependency b ~ a) => ResolveRef 'SINGLE m a b where
  resolveRef _ = Ref
